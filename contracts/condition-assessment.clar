;; Condition Assessment Contract
;; Tracks the physical condition and health of infrastructure assets

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_ASSET_NOT_FOUND (err u201))
(define-constant ERR_INVALID_SCORE (err u202))

;; Condition scores: 1-5 scale (1=Poor, 2=Fair, 3=Good, 4=Very Good, 5=Excellent)
(define-map asset-conditions
  { asset-id: uint }
  {
    structural-score: uint,
    functional-score: uint,
    safety-score: uint,
    last-assessment: uint,
    assessor: principal,
    notes: (string-ascii 200)
  }
)

;; Assessment history
(define-map assessment-history
  { asset-id: uint, assessment-id: uint }
  {
    structural-score: uint,
    functional-score: uint,
    safety-score: uint,
    assessment-date: uint,
    assessor: principal
  }
)

(define-data-var assessment-counter uint u0)

;; Authorized assessors
(define-map authorized-assessors principal bool)

;; Record condition assessment
(define-public (assess-condition (asset-id uint)
                                (structural-score uint)
                                (functional-score uint)
                                (safety-score uint)
                                (notes (string-ascii 200)))
  (let ((assessment-id (+ (var-get assessment-counter) u1)))
    (asserts! (is-authorized-assessor tx-sender) ERR_UNAUTHORIZED)
    (asserts! (and (<= structural-score u5) (>= structural-score u1)) ERR_INVALID_SCORE)
    (asserts! (and (<= functional-score u5) (>= functional-score u1)) ERR_INVALID_SCORE)
    (asserts! (and (<= safety-score u5) (>= safety-score u1)) ERR_INVALID_SCORE)

    ;; Update current condition
    (map-set asset-conditions
      { asset-id: asset-id }
      {
        structural-score: structural-score,
        functional-score: functional-score,
        safety-score: safety-score,
        last-assessment: block-height,
        assessor: tx-sender,
        notes: notes
      })

    ;; Store in history
    (map-set assessment-history
      { asset-id: asset-id, assessment-id: assessment-id }
      {
        structural-score: structural-score,
        functional-score: functional-score,
        safety-score: safety-score,
        assessment-date: block-height,
        assessor: tx-sender
      })

    (var-set assessment-counter assessment-id)
    (ok assessment-id)))

;; Get current condition
(define-read-only (get-condition (asset-id uint))
  (map-get? asset-conditions { asset-id: asset-id }))

;; Calculate overall condition score
(define-read-only (get-overall-score (asset-id uint))
  (match (get-condition asset-id)
    condition (let ((structural (get structural-score condition))
                    (functional (get functional-score condition))
                    (safety (get safety-score condition)))
                (ok (/ (+ structural functional safety) u3)))
    (err ERR_ASSET_NOT_FOUND)))

;; Authorization functions
(define-public (add-assessor (assessor principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-assessors assessor true)
    (ok true)))

(define-read-only (is-authorized-assessor (caller principal))
  (or (is-eq caller CONTRACT_OWNER)
      (default-to false (map-get? authorized-assessors caller))))
