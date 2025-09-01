;; GroveCraft - Decentralized Social Impact Platform

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-PROJECT-NOT-FOUND (err u102))
(define-constant ERR-PROJECT-ALREADY-EXISTS (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-MILESTONE-NOT-READY (err u105))
(define-constant ERR-INVALID-VALIDATION-TYPE (err u106))
(define-constant ERR-ALREADY-VALIDATED (err u107))
(define-constant ERR-PROJECT-COMPLETED (err u108))
(define-constant ERR-INVALID-REPUTATION (err u109))
(define-constant ERR-DISPUTE-EXISTS (err u110))
(define-constant ERR-VOTING-CLOSED (err u111))
(define-constant ERR-CROSS-POLLINATION-FAILED (err u112))

;; Data Variables
(define-data-var next-project-id uint u1)
(define-data-var platform-fee uint u250) ;; 2.5% in basis points
(define-data-var total-platform-revenue uint u0)
(define-data-var ai-prediction-threshold uint u75) ;; 75% success threshold
(define-data-var global-impact-score uint u0)

;; Data Maps
(define-map projects
  { project-id: uint }
  {
    creator: principal,
    title: (string-utf8 200),
    description: (string-utf8 1000),
    funding-goal: uint,
    current-funding: uint,
    community-region: (string-ascii 50),
    economic-indicator: uint,
    ai-prediction-score: uint,
    status: (string-ascii 20), ;; "funding", "active", "completed", "failed"
    created-at: uint,
    milestones-completed: uint,
    total-milestones: uint,
    impact-metrics: uint
  }
)

(define-map project-validations
  { project-id: uint, validation-type: (string-ascii 20) }
  {
    validator: principal,
    score: uint,
    validated-at: uint,
    data-hash: (buff 32),
    is-verified: bool
  }
)

(define-map user-contributions
  { contributor: principal, project-id: uint }
  {
    amount: uint,
    reputation-staked: uint,
    contributed-at: uint,
    rewards-earned: uint
  }
)

(define-map user-reputation
  { user: principal }
  {
    score: uint,
    successful-projects: uint,
    failed-projects: uint,
    total-staked: uint,
    available-stake: uint
  }
)

(define-map funding-pools
  { region: (string-ascii 50) }
  {
    total-funds: uint,
    min-contribution: uint,
    active-projects: uint,
    success-rate: uint,
    economic-multiplier: uint
  }
)

(define-map cross-pollination-links
  { source-project: uint, target-region: (string-ascii 50) }
  {
    spawned-project: uint,
    adaptation-score: uint,
    success-correlation: uint,
    created-at: uint
  }
)

(define-map project-disputes
  { project-id: uint }
  {
    disputer: principal,
    reason: (string-utf8 500),
    votes-for: uint,
    votes-against: uint,
    voting-deadline: uint,
    is-resolved: bool,
    resolution: (string-utf8 200)
  }
)

(define-map milestone-releases
  { project-id: uint, milestone: uint }
  {
    amount: uint,
    verification-hash: (buff 32),
    iot-data-verified: bool,
    community-approved: bool,
    expert-approved: bool,
    released-at: uint
  }
)

(define-map expert-reviewers
  { reviewer: principal }
  {
    expertise-areas: (list 10 (string-ascii 30)),
    reputation-score: uint,
    reviews-completed: uint,
    accuracy-rating: uint,
    is-active: bool
  }
)

(define-map ai-learning-data
  { pattern-id: uint }
  {
    project-type: (string-ascii 50),
    success-factors: (list 5 uint),
    optimal-funding: uint,
    recommended-duration: uint,
    impact-multiplier: uint
  }
)

;; Authorization Functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER))

(define-private (is-authorized-validator (project-id uint))
  (let ((project (unwrap! (map-get? projects { project-id: project-id }) false)))
    (or 
      (is-eq tx-sender (get creator project))
      (is-some (map-get? expert-reviewers { reviewer: tx-sender }))
    )
  )
)

;; Project Management Functions
(define-public (create-project 
  (title (string-utf8 200))
  (description (string-utf8 1000))
  (funding-goal uint)
  (region (string-ascii 50))
  (economic-indicator uint)
  (total-milestones uint))
  (let (
    (project-id (var-get next-project-id))
    (ai-score (calculate-ai-prediction funding-goal economic-indicator))
  )
    (asserts! (> funding-goal u0) ERR-INVALID-AMOUNT)
    (asserts! (> total-milestones u0) ERR-INVALID-AMOUNT)
    
    (map-set projects
      { project-id: project-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        funding-goal: funding-goal,
        current-funding: u0,
        community-region: region,
        economic-indicator: economic-indicator,
        ai-prediction-score: ai-score,
        status: "funding",
        created-at: block-height,
        milestones-completed: u0,
        total-milestones: total-milestones,
        impact-metrics: u0
      }
    )
    
    (var-set next-project-id (+ project-id u1))
    (update-regional-pool region funding-goal)
    (ok project-id)
  )
)

(define-public (contribute-to-project 
  (project-id uint) 
  (amount uint)
  (reputation-stake uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (user-rep (default-to 
      { score: u100, successful-projects: u0, failed-projects: u0, total-staked: u0, available-stake: u100 }
      (map-get? user-reputation { user: tx-sender })
    ))
    (current-contribution (default-to 
      { amount: u0, reputation-staked: u0, contributed-at: u0, rewards-earned: u0 }
      (map-get? user-contributions { contributor: tx-sender, project-id: project-id })
    ))
  )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (is-eq (get status project) "funding") ERR-PROJECT-COMPLETED)
    (asserts! (<= reputation-stake (get available-stake user-rep)) ERR-INVALID-REPUTATION)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set projects
      { project-id: project-id }
      (merge project { current-funding: (+ (get current-funding project) amount) })
    )
    
    (map-set user-contributions
      { contributor: tx-sender, project-id: project-id }
      {
        amount: (+ (get amount current-contribution) amount),
        reputation-staked: (+ (get reputation-staked current-contribution) reputation-stake),
        contributed-at: block-height,
        rewards-earned: (get rewards-earned current-contribution)
      }
    )
    
    (map-set user-reputation
      { user: tx-sender }
      (merge user-rep { 
        available-stake: (- (get available-stake user-rep) reputation-stake),
        total-staked: (+ (get total-staked user-rep) reputation-stake)
      })
    )
    
    (ok true)
  )
)

(define-public (validate-project-impact
  (project-id uint)
  (validation-type (string-ascii 20))
  (score uint)
  (data-hash (buff 32)))
  (let ((project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND)))
    (asserts! (is-authorized-validator project-id) ERR-UNAUTHORIZED)
    (asserts! (<= score u100) ERR-INVALID-AMOUNT)
    (asserts! 
      (is-none (map-get? project-validations { project-id: project-id, validation-type: validation-type }))
      ERR-ALREADY-VALIDATED
    )
    
    (map-set project-validations
      { project-id: project-id, validation-type: validation-type }
      {
        validator: tx-sender,
        score: score,
        validated-at: block-height,
        data-hash: data-hash,
        is-verified: true
      }
    )
    
    (ok true)
  )
)

(define-public (release-milestone-funding 
  (project-id uint) 
  (milestone uint)
  (verification-hash (buff 32)))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (funding-per-milestone (/ (get current-funding project) (get total-milestones project)))
  )
    (asserts! (is-eq tx-sender (get creator project)) ERR-UNAUTHORIZED)
    (asserts! (< milestone (get total-milestones project)) ERR-MILESTONE-NOT-READY)
    (asserts! 
      (is-none (map-get? milestone-releases { project-id: project-id, milestone: milestone }))
      ERR-ALREADY-VALIDATED
    )
    (asserts! (verify-triple-validation project-id) ERR-INVALID-VALIDATION-TYPE)
    
    (try! (as-contract (stx-transfer? funding-per-milestone tx-sender (get creator project))))
    
    (map-set milestone-releases
      { project-id: project-id, milestone: milestone }
      {
        amount: funding-per-milestone,
        verification-hash: verification-hash,
        iot-data-verified: true,
        community-approved: true,
        expert-approved: true,
        released-at: block-height
      }
    )
    
    (map-set projects
      { project-id: project-id }
      (merge project { milestones-completed: (+ (get milestones-completed project) u1) })
    )
    
    (ok funding-per-milestone)
  )
)

(define-public (create-cross-pollination
  (source-project-id uint)
  (target-region (string-ascii 50))
  (adaptation-params (list 5 uint)))
  (let (
    (source-project (unwrap! (map-get? projects