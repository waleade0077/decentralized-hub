(define-trait ft-trait
  (
    (transfer (uint principal principal) (response bool uint))
    (balance-of (principal) (response uint uint))
    (total-supply () (response uint uint))
    (decimals () (response uint uint))
    (name () (response (string-ascii 32) uint))
    (symbol () (response (string-ascii 32) uint))
  )
)

;; Constants
(define-constant DAO-QUORUM u3)
(define-constant STAKE-MIN u50000)
(define-constant REFUND-BLOCK-GAP u1000)

;; Helper functions
(define-private (contains-principal (haystack (list 10 principal)) (needle principal))
  (is-some (index-of haystack needle)))

(define-private (validate-job-id (job-id uint))
  (and (> job-id u0) (<= job-id (var-get job-counter))))

(define-private (validate-project-id (project-id uint))
  (and (> project-id u0) (<= project-id (var-get project-counter))))

(define-public (advance-block (blocks uint))
  (begin
    (asserts! (> blocks u0) (err u415))
    (var-set current-block (+ (var-get current-block) blocks))
    (ok true)))

;; === Global Data ===
(define-data-var job-counter uint u0)
(define-data-var project-counter uint u0)
(define-data-var current-block uint u0)
(define-map reputation principal int)
(define-map staked principal uint)
(define-map subscribers principal bool)

;; === Jobs ===
(define-map jobs
  uint
  {
    client: principal,
    freelancer: (optional principal),
    amount: uint,
    milestone: uint,
    paid: bool,
    approved: bool
  }
)

(define-map job-votes uint (list 10 principal))
(define-map job-auctions uint (tuple (min-bid uint) (highest-bidder (optional principal)) (end-block uint)))

;; === Projects ===
(define-map projects
  uint
  {
    creator: principal,
    goal: uint,
    pledged: uint,
    deadline: uint,
    milestone: uint,
    funded: bool,
    failed: bool
  }
)

(define-map project-votes uint (list 10 principal))
(define-map pledges
  { project-id: uint, backer: principal }
  uint
)

;; === Subscription and Staking ===
(define-public (subscribe)
  (begin
    (map-set subscribers tx-sender true)
    (ok true)
  )
)

(define-public (stake)
  (let ((bal (stx-get-balance tx-sender)))
    (if (< bal STAKE-MIN)
      (err u401)
      (match (stx-transfer? STAKE-MIN tx-sender (as-contract tx-sender))
        success
          (begin
            (map-set staked tx-sender STAKE-MIN)
            (ok true)
          )
        error (err error)
      )
    )
  )
)

;; === Freelance Job Board ===
(define-public (post-job (amount uint))
  (if (> amount u0)
    (let ((id (+ (var-get job-counter) u1)))
      (begin
        (var-set job-counter id)
        (map-set jobs id {
          client: tx-sender,
          freelancer: none,
          amount: amount,
          milestone: u0,
          paid: false,
          approved: false
        })
        (ok id)
      ))
    (err u400)))

(define-public (bid-job (job-id uint))
  (begin
    (asserts! (validate-job-id job-id) (err u416))
    (let ((job (unwrap! (map-get? jobs job-id) (err u100))))
      (asserts! (is-none (get freelancer job)) (err u401))
      (ok (map-set jobs job-id (merge job { freelancer: (some tx-sender) }))))))

(define-public (fund-job (job-id uint))
  (let ((job (map-get? jobs job-id)))
    (match job data
      (if (is-eq tx-sender (get client data))
        (match (stx-transfer? (get amount data) tx-sender (as-contract tx-sender))
          success (ok true)
          error (err error)
        )
        (err u101)
      )
      (err u102)
    )
  )
)

(define-public (submit-milestone (job-id uint))
  (begin
    (asserts! (validate-job-id job-id) (err u416))
    (let ((job (unwrap! (map-get? jobs job-id) (err u102))))
      (let ((freelancer-opt (get freelancer job))
            (current-milestone (get milestone job))
            (max-milestones u5))
        (asserts! (is-some freelancer-opt) (err u402))
        (asserts! (is-eq tx-sender (unwrap! freelancer-opt (err u403))) (err u103))
        (asserts! (< current-milestone max-milestones) (err u417))
        (ok (map-set jobs job-id 
          { 
            client: (get client job),
            freelancer: freelancer-opt,
            amount: (get amount job),
            milestone: (+ current-milestone u1),
            paid: (get paid job),
            approved: (get approved job)
          }))))))

(define-public (vote-approve-job (job-id uint))
  (let ((job (map-get? jobs job-id)))
    (match job data
      (let ((voters (default-to (list) (map-get? job-votes job-id))))
        (if (contains-principal voters tx-sender)
          (err u104)
          (let ((new-votes (unwrap-panic (as-max-len? (concat voters (list tx-sender)) u10))))
            (begin
              (map-set job-votes job-id new-votes)
              (if (>= (len new-votes) DAO-QUORUM)
                (match (stx-transfer? (get amount data) (as-contract tx-sender) (unwrap-panic (get freelancer data)))
                  success
                    (begin
                      (map-set jobs job-id (merge data { paid: true, approved: true }))
                      (ok true)
                    )
                  error (err error)
                )
                (ok false)
              )
            )
          )
        )
      )
      (err u102)
    )
  )
)

;; === Crowdfunding Launchpad ===
(define-public (create-project (goal uint) (deadline uint))
  (let 
    ((current-height (var-get current-block)))
    (asserts! (> goal u0) (err u404))
    (asserts! (> deadline current-height) (err u405))
    (let ((id (+ (var-get project-counter) u1)))
      (begin
        (var-set project-counter id)
        (map-set projects id {
          creator: tx-sender,
          goal: goal,
          pledged: u0,
          deadline: deadline,
          milestone: u0,
          funded: false,
          failed: false
        })
        (ok id)))))

(define-public (pledge (project-id uint) (amount uint))
  (begin
    (asserts! (validate-project-id project-id) (err u418))
    (let ((project (unwrap! (map-get? projects project-id) (err u106))))
      (let ((deadline-height (get deadline project))
            (current-height (var-get current-block))
            (existing-pledge (default-to u0 (map-get? pledges { project-id: project-id, backer: tx-sender })))
            (project-goal (get goal project)))
        (asserts! (> amount u0) (err u407))
        (asserts! (<= current-height deadline-height) (err u105))
        (asserts! (not (get funded project)) (err u419))
        (asserts! (not (get failed project)) (err u420))
        (let ((new-pledge (+ existing-pledge amount))
              (new-total (+ (get pledged project) amount)))
          (asserts! (<= new-total project-goal) (err u421))
          (match (stx-transfer? amount tx-sender (as-contract tx-sender))
            success
              (begin
                (map-set pledges { project-id: project-id, backer: tx-sender } new-pledge)
                (map-set projects project-id 
                  (merge project { pledged: new-total }))
                (ok true))
            error (err error)))))))

(define-public (vote-project-release (project-id uint))
  (begin
    (asserts! (validate-project-id project-id) (err u418))
    (let ((project (unwrap! (map-get? projects project-id) (err u106))))
      (let ((votes (default-to (list) (map-get? project-votes project-id)))
            (pledged-amount (get pledged project))
            (project-goal (get goal project)))
        (asserts! (not (get funded project)) (err u419))
        (asserts! (>= pledged-amount project-goal) (err u422))
        (asserts! (not (contains-principal votes tx-sender)) (err u107))
        (let ((new-votes (unwrap! (as-max-len? (concat votes (list tx-sender)) u10) (err u423))))
          (begin
            (map-set project-votes project-id new-votes)
            (if (>= (len new-votes) DAO-QUORUM)
              (match (stx-transfer? (/ pledged-amount u3) (as-contract tx-sender) (get creator project))
                success (begin
                  (map-set projects project-id 
                    (merge project { 
                      milestone: (+ (get milestone project) u1),
                      funded: true 
                    }))
                  (ok true))
                error (err error))
              (ok false))))))))

(define-public (refund-project (project-id uint))
  (let ((project (map-get? projects project-id)))
    (match project p
      (let ((pledged-amt (map-get? pledges { project-id: project-id, backer: tx-sender })))
        (match pledged-amt amount
          (if (and (> (var-get current-block) (get deadline p)) (not (get funded p)))
            (match (stx-transfer? amount (as-contract tx-sender) tx-sender)
              success (begin
                (map-delete pledges { project-id: project-id, backer: tx-sender })
                (map-set projects project-id (merge p { failed: true }))
                (ok true))
              error (err error))
            (err u108))
          (err u109)))
      (err u106))))

;; === Reputation ===
(define-public (rate-user (user principal) (score int))
  (begin
    (asserts! (and (>= score (- 0 5)) (<= score 5)) (err u408))
    (let ((current (unwrap-panic (map-get? reputation user))))
      (let ((new-score (+ current score)))
        (map-set reputation user new-score)
        (ok true)))))

;; === Auction ===
(define-public (start-job-auction (job-id uint) (min-bid uint) (end-block uint))
  (begin
    (asserts! (validate-job-id job-id) (err u416))
    (let ((current-height (var-get current-block))
          (job (unwrap! (map-get? jobs job-id) (err u100))))
      (asserts! (> min-bid u0) (err u410))
      (asserts! (> end-block current-height) (err u411))
      (asserts! (is-none (get freelancer job)) (err u424))
      (ok (map-set job-auctions job-id 
        { 
          min-bid: min-bid,
          highest-bidder: none,
          end-block: end-block
        })))))

(define-public (bid-auction (job-id uint) (bid uint))
  (begin
    (asserts! (validate-job-id job-id) (err u416))
    (let ((auction (unwrap! (map-get? job-auctions job-id) (err u110)))
          (current-height (var-get current-block)))
      (let ((auction-end (get end-block auction))
            (current-bid (get min-bid auction))
            (current-winner (get highest-bidder auction)))
        (asserts! (<= current-height auction-end) (err u412))
        (asserts! (>= bid current-bid) (err u413))
        (asserts! (not (is-eq (some tx-sender) current-winner)) (err u414))
        (ok (map-set job-auctions job-id 
          { 
            min-bid: bid,
            highest-bidder: (some tx-sender),
            end-block: auction-end
          }))))))

;; === Read-only ===
(define-read-only (get-job (id uint)) 
  (ok (map-get? jobs id)))

(define-read-only (get-project (id uint)) 
  (ok (map-get? projects id)))

(define-read-only (get-auction (id uint)) 
  (ok (map-get? job-auctions id)))

(define-read-only (get-reputation (user principal)) 
  (ok (default-to 0 (map-get? reputation user))))

(define-read-only (get-subscription (user principal)) 
  (ok (default-to false (map-get? subscribers user))))

;; === Block Management ===
(define-read-only (get-current-block) 
  (ok (var-get current-block)))
