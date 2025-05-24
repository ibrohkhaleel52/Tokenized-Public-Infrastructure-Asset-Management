;; Maintenance Planning Contract
;; Schedules and tracks maintenance activities for infrastructure assets

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_INVALID_INPUT (err u301))
(define-constant ERR_TASK_NOT_FOUND (err u302))

;; Maintenance task statuses
(define-constant STATUS_PLANNED u1)
(define-constant STATUS_IN_PROGRESS u2)
(define-constant STATUS_COMPLETED u3)
(define-constant STATUS_CANCELLED u4)

;; Maintenance tasks
(define-map maintenance-tasks
  { task-id: uint }
  {
    asset-id: uint,
    task-type: (string-ascii 30),
    description: (string-ascii 200),
    scheduled-date: uint,
    estimated-cost: uint,
    priority: uint,
    status: uint,
    assigned-to: principal,
    created-by: principal,
    completion-date: (optional uint)
  }
)

(define-data-var task-counter uint u0)

;; Authorized planners
(define-map authorized-planners principal bool)

;; Create maintenance task
(define-public (create-task (asset-id uint)
                           (task-type (string-ascii 30))
                           (description (string-ascii 200))
                           (scheduled-date uint)
                           (estimated-cost uint)
                           (priority uint)
                           (assigned-to principal))
  (let ((new-task-id (+ (var-get task-counter) u1)))
    (asserts! (is-authorized-planner tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> (len task-type) u0) ERR_INVALID_INPUT)
    (asserts! (and (<= priority u5) (>= priority u1)) ERR_INVALID_INPUT)

    (map-set maintenance-tasks
      { task-id: new-task-id }
      {
        asset-id: asset-id,
        task-type: task-type,
        description: description,
        scheduled-date: scheduled-date,
        estimated-cost: estimated-cost,
        priority: priority,
        status: STATUS_PLANNED,
        assigned-to: assigned-to,
        created-by: tx-sender,
        completion-date: none
      })

    (var-set task-counter new-task-id)
    (ok new-task-id)))

;; Update task status
(define-public (update-task-status (task-id uint) (new-status uint))
  (let ((task (unwrap! (get-task task-id) ERR_TASK_NOT_FOUND)))
    (asserts! (or (is-authorized-planner tx-sender)
                  (is-eq tx-sender (get assigned-to task))) ERR_UNAUTHORIZED)

    (let ((updated-task (if (is-eq new-status STATUS_COMPLETED)
                           (merge task { status: new-status, completion-date: (some block-height) })
                           (merge task { status: new-status }))))
      (map-set maintenance-tasks { task-id: task-id } updated-task)
      (ok true))))

;; Get task information
(define-read-only (get-task (task-id uint))
  (map-get? maintenance-tasks { task-id: task-id }))

;; Get tasks by asset
(define-read-only (get-tasks-by-asset (asset-id uint))
  ;; This is a simplified implementation
  ;; In practice, you'd want to implement pagination
  (ok asset-id))

;; Authorization functions
(define-public (add-planner (planner principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-planners planner true)
    (ok true)))

(define-read-only (is-authorized-planner (caller principal))
  (or (is-eq caller CONTRACT_OWNER)
      (default-to false (map-get? authorized-planners caller))))

;; Get total tasks
(define-read-only (get-total-tasks)
  (var-get task-counter))
