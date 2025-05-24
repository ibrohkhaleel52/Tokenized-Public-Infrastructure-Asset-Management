;; Investment Prioritization Contract
;; Ranks infrastructure investment needs based on various criteria

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_INVALID_INPUT (err u401))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u402))

;; Investment proposal statuses
(define-constant STATUS_PROPOSED u1)
(define-constant STATUS_UNDER_REVIEW u2)
(define-constant STATUS_APPROVED u3)
(define-constant STATUS_REJECTED u4)
(define-constant STATUS_FUNDED u5)

;; Investment proposals
(define-map investment-proposals
  { proposal-id: uint }
  {
    asset-id: uint,
    project-name: (string-ascii 100),
    description: (string-ascii 300),
    requested-amount: uint,
    urgency-score: uint,
    impact-score: uint,
    risk-score: uint,
    status: uint,
    proposer: principal,
    created-at: uint,
    review-notes: (string-ascii 200)
  }
)

(define-data-var proposal-counter uint u0)

;; Authorized reviewers
(define-map authorized-reviewers principal bool)

;; Submit investment proposal
(define-public (submit-proposal (asset-id uint)
                               (project-name (string-ascii 100))
                               (description (string-ascii 300))
                               (requested-amount uint)
                               (urgency-score uint)
                               (impact-score uint)
                               (risk-score uint))
  (let ((new-proposal-id (+ (var-get proposal-counter) u1)))
    (asserts! (> (len project-name) u0) ERR_INVALID_INPUT)
    (asserts! (> requested-amount u0) ERR_INVALID_INPUT)
    (asserts! (and (<= urgency-score u10) (>= urgency-score u1)) ERR_INVALID_INPUT)
    (asserts! (and (<= impact-score u10) (>= impact-score u1)) ERR_INVALID_INPUT)
    (asserts! (and (<= risk-score u10) (>= risk-score u1)) ERR_INVALID_INPUT)

    (map-set investment-proposals
      { proposal-id: new-proposal-id }
      {
        asset-id: asset-id,
        project-name: project-name,
        description: description,
        requested-amount: requested-amount,
        urgency-score: urgency-score,
        impact-score: impact-score,
        risk-score: risk-score,
        status: STATUS_PROPOSED,
        proposer: tx-sender,
        created-at: block-height,
        review-notes: ""
      })

    (var-set proposal-counter new-proposal-id)
    (ok new-proposal-id)))

;; Review proposal
(define-public (review-proposal (proposal-id uint)
                               (new-status uint)
                               (review-notes (string-ascii 200)))
  (let ((proposal (unwrap! (get-proposal proposal-id) ERR_PROPOSAL_NOT_FOUND)))
    (asserts! (is-authorized-reviewer tx-sender) ERR_UNAUTHORIZED)

    (map-set investment-proposals
      { proposal-id: proposal-id }
      (merge proposal {
        status: new-status,
        review-notes: review-notes
      }))
    (ok true)))

;; Calculate priority score
(define-read-only (calculate-priority-score (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal (let ((urgency (get urgency-score proposal))
                   (impact (get impact-score proposal))
                   (risk (get risk-score proposal)))
               ;; Priority = (urgency * 0.4) + (impact * 0.4) + ((11 - risk) * 0.2)
               ;; Higher urgency and impact = higher priority
               ;; Higher risk = lower priority
               (ok (+ (/ (* urgency u40) u100)
                      (+ (/ (* impact u40) u100)
                         (/ (* (- u11 risk) u20) u100)))))
    (err ERR_PROPOSAL_NOT_FOUND)))

;; Get proposal information
(define-read-only (get-proposal (proposal-id uint))
  (map-get? investment-proposals { proposal-id: proposal-id }))

;; Authorization functions
(define-public (add-reviewer (reviewer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-reviewers reviewer true)
    (ok true)))

(define-read-only (is-authorized-reviewer (caller principal))
  (or (is-eq caller CONTRACT_OWNER)
      (default-to false (map-get? authorized-reviewers caller))))

;; Get total proposals
(define-read-only (get-total-proposals)
  (var-get proposal-counter))
