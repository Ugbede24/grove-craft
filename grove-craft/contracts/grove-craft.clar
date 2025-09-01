;; GroveCraft - Simplified Decentralized Social Impact Platform

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-PROJECT-NOT-FOUND (err u102))
(define-constant ERR-PROJECT-ALREADY-EXISTS (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-MILESTONE-NOT-READY (err u105))
(define-constant ERR-ALREADY-VALIDATED (err u106))
(define-constant ERR-PROJECT-COMPLETED (err u107))
(define-constant ERR-INVALID-STATUS (err u108))

;; Data Variables
(define-data-var next-project-id uint u1)
(define-data-var platform-fee uint u250) ;; 2.5% in basis points (250/10000)
(define-data-var total-platform-revenue uint u0)

;; Data Maps
(define-map projects
  { project-id: uint }
  {
    creator: principal,
    title: (string-utf8 200),
    description: (string-utf8 500),
    funding-goal: uint,
    current-funding: uint,
    region: (string-ascii 50),
    status: (string-ascii 20), ;; "funding", "active", "completed", "failed"
    created-at: uint,
    milestones-completed: uint,
    total-milestones: uint
  }
)

(define-map user-contributions
  { contributor: principal, project-id: uint }
  {
    amount: uint,
    contributed-at: uint
  }
)

(define-map user-reputation
  { user: principal }
  {
    score: uint,
    successful-projects: uint,
    total-contributed: uint
  }
)

(define-map milestone-releases
  { project-id: uint, milestone: uint }
  {
    amount: uint,
    released-at: uint,
    is-released: bool
  }
)

;; Authorization Functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER))

(define-private (is-project-creator (project-id uint))
  (match (map-get? projects { project-id: project-id })
    project (is-eq tx-sender (get creator project))
    false
  )
)

;; Helper Functions
(define-private (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee)) u10000)
)

(define-private (update-user-reputation (user principal) (amount uint) (success bool))
  (let (
    (current-rep (default-to 
      { score: u100, successful-projects: u0, total-contributed: u0 }
      (map-get? user-reputation { user: user })
    ))
  )
    (map-set user-reputation
      { user: user }
      {
        score: (if success 
          (+ (get score current-rep) u10) 
          (if (> (get score current-rep) u10) (- (get score current-rep) u10) u0)
        ),
        successful-projects: (if success (+ (get successful-projects current-rep) u1) (get successful-projects current-rep)),
        total-contributed: (+ (get total-contributed current-rep) amount)
      }
    )
  )
)

;; Core Functions
(define-public (create-project 
  (title (string-utf8 200))
  (description (string-utf8 500))
  (funding-goal uint)
  (region (string-ascii 50))
  (total-milestones uint))
  (let ((project-id (var-get next-project-id)))
    (asserts! (> funding-goal u0) ERR-INVALID-AMOUNT)
    (asserts! (> total-milestones u0) ERR-INVALID-AMOUNT)
    (asserts! (< (len title) u201) ERR-INVALID-AMOUNT)
    (asserts! (< (len description) u501) ERR-INVALID-AMOUNT)
    (asserts! (< (len region) u51) ERR-INVALID-AMOUNT)
    
    (map-set projects
      { project-id: project-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        funding-goal: funding-goal,
        current-funding: u0,
        region: region,
        status: "funding",
        created-at: block-height,
        milestones-completed: u0,
        total-milestones: total-milestones
      }
    )
    
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

(define-public (contribute-to-project (project-id uint) (amount uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (fee (calculate-platform-fee amount))
    (net-amount (- amount fee))
    (current-contribution (default-to 
      { amount: u0, contributed-at: u0 }
      (map-get? user-contributions { contributor: tx-sender, project-id: project-id })
    ))
  )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (is-eq (get status project) "funding") ERR-PROJECT-COMPLETED)
    
    ;; Transfer funds to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update platform revenue
    (var-set total-platform-revenue (+ (var-get total-platform-revenue) fee))
    
    ;; Update project funding
    (map-set projects
      { project-id: project-id }
      (merge project { current-funding: (+ (get current-funding project) net-amount) })
    )
    
    ;; Update user contribution
    (map-set user-contributions
      { contributor: tx-sender, project-id: project-id }
      {
        amount: (+ (get amount current-contribution) net-amount),
        contributed-at: block-height
      }
    )
    
    ;; Update user reputation
    (update-user-reputation tx-sender net-amount true)
    
    ;; Check if funding goal is reached
    (if (>= (+ (get current-funding project) net-amount) (get funding-goal project))
      (begin
        (map-set projects
          { project-id: project-id }
          (merge project { 
            current-funding: (+ (get current-funding project) net-amount),
            status: "active" 
          })
        )
        (ok { project-activated: true, contribution-amount: net-amount })
      )
      (ok { project-activated: false, contribution-amount: net-amount })
    )
  )
)

(define-public (release-milestone-funding (project-id uint) (milestone uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (milestone-amount (/ (get current-funding project) (get total-milestones project)))
  )
    (asserts! (is-project-creator project-id) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status project) "active") ERR-INVALID-STATUS)
    (asserts! (< milestone (get total-milestones project)) ERR-MILESTONE-NOT-READY)
    (asserts! (is-eq milestone (get milestones-completed project)) ERR-MILESTONE-NOT-READY)
    (asserts! 
      (is-none (map-get? milestone-releases { project-id: project-id, milestone: milestone }))
      ERR-ALREADY-VALIDATED
    )
    
    ;; Release milestone funding
    (try! (as-contract (stx-transfer? milestone-amount tx-sender (get creator project))))
    
    ;; Record milestone release
    (map-set milestone-releases
      { project-id: project-id, milestone: milestone }
      {
        amount: milestone-amount,
        released-at: block-height,
        is-released: true
      }
    )
    
    ;; Update project milestones
    (let ((new-milestones-completed (+ (get milestones-completed project) u1)))
      (map-set projects
        { project-id: project-id }
        (merge project { milestones-completed: new-milestones-completed })
      )
      
      ;; Check if project is completed
      (if (is-eq new-milestones-completed (get total-milestones project))
        (begin
          (map-set projects
            { project-id: project-id }
            (merge project { 
              milestones-completed: new-milestones-completed,
              status: "completed" 
            })
          )
          (reward-contributors project-id)
          (ok { milestone-released: milestone-amount, project-completed: true })
        )
        (ok { milestone-released: milestone-amount, project-completed: false })
      )
    )
  )
)

(define-private (reward-contributors (project-id uint))
  (let ((project (unwrap! (map-get? projects { project-id: project-id }) false)))
    ;; Simple reward mechanism - in a real implementation, you'd iterate through contributors
    ;; For now, just update the creator's reputation
    (update-user-reputation (get creator project) (get current-funding project) true)
    true
  )
)

;; Read-only Functions
(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

(define-read-only (get-user-contribution (contributor principal) (project-id uint))
  (map-get? user-contributions { contributor: contributor, project-id: project-id })
)

(define-read-only (get-user-reputation (user principal))
  (map-get? user-reputation { user: user })
)

(define-read-only (get-platform-stats)
  {
    total-projects: (- (var-get next-project-id) u1),
    platform-fee: (var-get platform-fee),
    total-revenue: (var-get total-platform-revenue)
  }
)

(define-read-only (get-milestone-info (project-id uint) (milestone uint))
  (map-get? milestone-releases { project-id: project-id, milestone: milestone })
)

(define-read-only (calculate-project-progress (project-id uint))
  (match (map-get? projects { project-id: project-id })
    project (some {
      funding-progress: (if (> (get funding-goal project) u0)
        (/ (* (get current-funding project) u100) (get funding-goal project))
        u0
      ),
      milestone-progress: (if (> (get total-milestones project) u0)
        (/ (* (get milestones-completed project) u100) (get total-milestones project))
        u0
      )
    })
    none
  )
)

;; Admin Functions
(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
    (asserts! (<= new-fee u1000) ERR-INVALID-AMOUNT) ;; Max 10% fee
    (var-set platform-fee new-fee)
    (ok true)
  )
)

(define-public (withdraw-platform-revenue (amount uint))
  (begin
    (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
    (asserts! (<= amount (var-get total-platform-revenue)) ERR-INSUFFICIENT-FUNDS)
    
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER)))
    (var-set total-platform-revenue (- (var-get total-platform-revenue) amount))
    (ok amount)
  )
)

;; Emergency Functions
(define-public (emergency-pause-project (project-id uint))
  (let ((project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND)))
    (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
    
    (map-set projects
      { project-id: project-id }
      (merge project { status: "failed" })
    )
    (ok true)
  )
)

;; Utility Functions
(define-private (verify-triple-validation (project-id uint))
  ;; Simplified validation - in production, this would check multiple validation sources
  true
)

(define-private (update-regional-pool (region (string-ascii 50)) (amount uint))
  ;; Simplified regional pool update
  true
)