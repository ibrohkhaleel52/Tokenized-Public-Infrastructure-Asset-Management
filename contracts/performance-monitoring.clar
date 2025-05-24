;; Performance Monitoring Contract
;; Tracks asset utilization and performance metrics

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u500))
(define-constant ERR_INVALID_INPUT (err u501))
(define-constant ERR_RECORD_NOT_FOUND (err u502))

;; Performance metrics
(define-map performance-records
  { asset-id: uint, record-id: uint }
  {
    utilization-rate: uint,
    efficiency-score: uint,
    downtime-hours: uint,
    maintenance-cost: uint,
    user-satisfaction: uint,
    measurement-date: uint,
    recorded-by: principal
  }
)

;; Latest performance data
(define-map latest-performance
  { asset-id: uint }
  {
    utilization-rate: uint,
    efficiency-score: uint,
    downtime-hours: uint,
    maintenance-cost: uint,
    user-satisfaction: uint,
    last-updated: uint
  }
)

(define-data-var record-counter uint u0)

;; Authorized monitors
(define-map authorized-monitors principal bool)

;; Record performance metrics
(define-public (record-performance (asset-id uint)
                                  (utilization-rate uint)
                                  (efficiency-score uint)
                                  (downtime-hours uint)
                                  (maintenance-cost uint)
                                  (user-satisfaction uint))
  (let ((new-record-id (+ (var-get record-counter) u1)))
    (asserts! (is-authorized-monitor tx-sender) ERR_UNAUTHORIZED)
    (asserts! (<= utilization-rate u100) ERR_INVALID_INPUT)
    (asserts! (and (<= efficiency-score u100) (>= efficiency-score u0)) ERR_INVALID_INPUT)
    (asserts! (and (<= user-satisfaction u10) (>= user-satisfaction u1)) ERR_INVALID_INPUT)

    ;; Store detailed record
    (map-set performance-records
      { asset-id: asset-id, record-id: new-record-id }
      {
        utilization-rate: utilization-rate,
        efficiency-score: efficiency-score,
        downtime-hours: downtime-hours,
        maintenance-cost: maintenance-cost,
        user-satisfaction: user-satisfaction,
        measurement-date: block-height,
        recorded-by: tx-sender
      })

    ;; Update latest performance
    (map-set latest-performance
      { asset-id: asset-id }
      {
        utilization-rate: utilization-rate,
        efficiency-score: efficiency-score,
        downtime-hours: downtime-hours,
        maintenance-cost: maintenance-cost,
        user-satisfaction: user-satisfaction,
        last-updated: block-height
      })

    (var-set record-counter new-record-id)
    (ok new-record-id)))

;; Get latest performance data
(define-read-only (get-latest-performance (asset-id uint))
  (map-get? latest-performance { asset-id: asset-id }))

;; Get historical performance record
(define-read-only (get-performance-record (asset-id uint) (record-id uint))
  (map-get? performance-records { asset-id: asset-id, record-id: record-id }))

;; Calculate performance index
(define-read-only (calculate-performance-index (asset-id uint))
  (match (get-latest-performance asset-id)
    performance (let ((utilization (get utilization-rate performance))
                      (efficiency (get efficiency-score performance))
                      (satisfaction (get user-satisfaction performance)))
                  ;; Performance Index = (utilization * 0.3) + (efficiency * 0.4) + (satisfaction * 10 * 0.3)
                  (ok (+ (/ (* utilization u30) u100)
                         (+ (/ (* efficiency u40) u100)
                            (/ (* satisfaction u300) u100)))))
    (err ERR_RECORD_NOT_FOUND)))

;; Authorization functions
(define-public (add-monitor (monitor principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-monitors monitor true)
    (ok true)))

(define-read-only (is-authorized-monitor (caller principal))
  (or (is-eq caller CONTRACT_OWNER)
      (default-to false (map-get? authorized-monitors caller))))

;; Get total records
(define-read-only (get-total-records)
  (var-get record-counter))
