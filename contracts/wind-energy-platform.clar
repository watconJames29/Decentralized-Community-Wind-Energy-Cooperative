;; ===================================
;; WIND ENERGY COOPERATIVE SYSTEM
;; ===================================

;; contracts/wind-energy-management.clar
;; Main contract for turbine management and energy distribution

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_TURBINE_NOT_FOUND (err u101))
(define-constant ERR_INVALID_ENERGY_DATA (err u102))
(define-constant ERR_MAINTENANCE_REQUIRED (err u103))
(define-constant ERR_INSUFFICIENT_BALANCE (err u104))
(define-constant ERR_INVALID_DISTRIBUTION (err u105))

;; Turbine data structure
(define-map turbines
    { turbine-id: uint }
    {
        location: (string-ascii 100),
        capacity-kw: uint,
        installation-block: uint,
        status: (string-ascii 20), ;; "active", "maintenance", "offline"
        last-maintenance: uint,
        maintenance-interval: uint, ;; blocks between maintenance
        efficiency-rating: uint, ;; percentage (0-100)
        environmental-score: uint ;; environmental impact score (0-100)
    }
)

;; Energy production tracking
(define-map energy-production
    { turbine-id: uint, block-period: uint }
    {
        kwh-produced: uint,
        grid-delivered: uint,
        community-consumed: uint,
        efficiency-actual: uint,
        wind-speed-avg: uint,
        carbon-offset-kg: uint
    }
)

;; Grid integration data
(define-map grid-integration
    { block-period: uint }
    {
        total-production: uint,
        grid-demand: uint,
        price-per-kwh: uint,
        grid-stability-score: uint,
        peak-load-contribution: uint
    }
)

;; Community energy allocation
(define-map member-energy-allocation
    { member: principal, block-period: uint }
    {
        allocated-kwh: uint,
        consumed-kwh: uint,
        credits-earned: uint,
        payment-due: uint
    }
)

;; Environmental impact tracking
(define-map environmental-metrics
    { block-period: uint }
    {
        total-carbon-offset: uint,
        wildlife-impact-score: uint, ;; 0-100, lower is better
        noise-level-db: uint,
        land-use-efficiency: uint,
        biodiversity-index: uint
    }
)

;; Maintenance schedules and records
(define-map maintenance-records
    { turbine-id: uint, maintenance-id: uint }
    {
        scheduled-block: uint,
        completed-block: (optional uint),
        maintenance-type: (string-ascii 50),
        cost-stx: uint,
        performed-by: (optional principal),
        efficiency-impact: int, ;; can be negative or positive
        notes: (string-ascii 200)
    }
)

;; Global system state
(define-data-var total-turbines uint u0)
(define-data-var next-turbine-id uint u1)
(define-data-var next-maintenance-id uint u1)
(define-data-var system-active bool true)
(define-data-var emergency-shutdown bool false)

;; ===================================
;; TURBINE MANAGEMENT FUNCTIONS
;; ===================================

(define-public (register-turbine (location (string-ascii 100))
                                (capacity-kw uint)
                                (maintenance-interval uint))
    (let ((turbine-id (var-get next-turbine-id)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> capacity-kw u0) ERR_INVALID_ENERGY_DATA)

        (map-set turbines
            { turbine-id: turbine-id }
            {
                location: location,
                capacity-kw: capacity-kw,
                installation-block: stacks-block-height,
                status: "active",
                last-maintenance: stacks-block-height,
                maintenance-interval: maintenance-interval,
                efficiency-rating: u95, ;; start with 95% efficiency
                environmental-score: u85 ;; default environmental score
            }
        )

        (var-set next-turbine-id (+ turbine-id u1))
        (var-set total-turbines (+ (var-get total-turbines) u1))
        (ok turbine-id)
    )
)

(define-public (update-turbine-status (turbine-id uint) (new-status (string-ascii 20)))
    (let ((turbine-data (unwrap! (map-get? turbines { turbine-id: turbine-id }) ERR_TURBINE_NOT_FOUND)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)

        (map-set turbines
            { turbine-id: turbine-id }
            (merge turbine-data { status: new-status })
        )
        (ok true)
    )
)

;; ===================================
;; ENERGY PRODUCTION & DISTRIBUTION
;; ===================================

(define-public (record-energy-production (turbine-id uint)
                                       (kwh-produced uint)
                                       (wind-speed-avg uint))
    (let (
        (turbine-data (unwrap! (map-get? turbines { turbine-id: turbine-id }) ERR_TURBINE_NOT_FOUND))
        (current-period (/ stacks-block-height u144)) ;; ~24 hour periods
        (carbon-offset (* kwh-produced u1)) ;; 1kg CO2 per kWh simplified
    )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status turbine-data) "active") ERR_MAINTENANCE_REQUIRED)
        (asserts! (> kwh-produced u0) ERR_INVALID_ENERGY_DATA)

        ;; Calculate actual efficiency
        (let ((theoretical-max (* (get capacity-kw turbine-data) u24)) ;; 24 hours max
              (efficiency-actual (/ (* kwh-produced u100) theoretical-max)))

            (map-set energy-production
                { turbine-id: turbine-id, block-period: current-period }
                {
                    kwh-produced: kwh-produced,
                    grid-delivered: (/ (* kwh-produced u70) u100), ;; 70% to grid
                    community-consumed: (/ (* kwh-produced u30) u100), ;; 30% for community
                    efficiency-actual: efficiency-actual,
                    wind-speed-avg: wind-speed-avg,
                    carbon-offset-kg: carbon-offset
                }
            )

            ;; Update turbine efficiency rating
            (map-set turbines
                { turbine-id: turbine-id }
                (merge turbine-data { efficiency-rating: efficiency-actual })
            )

            (ok true)
        )
    )
)

(define-public (optimize-grid-integration (total-production uint)
                                        (grid-demand uint)
                                        (price-per-kwh uint))
    (let ((current-period (/ stacks-block-height u144)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)

        (let (
            (supply-demand-ratio (if (> grid-demand u0)
                                   (/ (* total-production u100) grid-demand)
                                   u0))
            (stability-score (if (and (>= supply-demand-ratio u80)
                                    (<= supply-demand-ratio u120))
                               u100
                               u50))
        )
            (map-set grid-integration
                { block-period: current-period }
                {
                    total-production: total-production,
                    grid-demand: grid-demand,
                    price-per-kwh: price-per-kwh,
                    grid-stability-score: stability-score,
                    peak-load-contribution: (/ (* total-production u15) u100) ;; 15% peak contribution
                }
            )
            (ok stability-score)
        )
    )
)

;; ===================================
;; MAINTENANCE MANAGEMENT
;; ===================================

(define-public (schedule-maintenance (turbine-id uint)
                                   (maintenance-type (string-ascii 50))
                                   (scheduled-blocks-ahead uint)
                                   (estimated-cost uint))
    (let (
        (turbine-data (unwrap! (map-get? turbines { turbine-id: turbine-id }) ERR_TURBINE_NOT_FOUND))
        (maintenance-id (var-get next-maintenance-id))
        (scheduled-block (+ stacks-block-height scheduled-blocks-ahead))
    )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)

        (map-set maintenance-records
            { turbine-id: turbine-id, maintenance-id: maintenance-id }
            {
                scheduled-block: scheduled-block,
                completed-block: none,
                maintenance-type: maintenance-type,
                cost-stx: estimated-cost,
                performed-by: none,
                efficiency-impact: 0,
                notes: ""
            }
        )

        (var-set next-maintenance-id (+ maintenance-id u1))
        (ok maintenance-id)
    )
)

(define-public (complete-maintenance (turbine-id uint)
                                   (maintenance-id uint)
                                   (actual-cost uint)
                                   (efficiency-impact int)
                                   (notes (string-ascii 200)))
    (let (
        (maintenance-data (unwrap! (map-get? maintenance-records
                                           { turbine-id: turbine-id, maintenance-id: maintenance-id })
                                  ERR_TURBINE_NOT_FOUND))
        (turbine-data (unwrap! (map-get? turbines { turbine-id: turbine-id }) ERR_TURBINE_NOT_FOUND))
    )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)

        ;; Update maintenance record
        (map-set maintenance-records
            { turbine-id: turbine-id, maintenance-id: maintenance-id }
            (merge maintenance-data {
                completed-block: (some stacks-block-height),
                cost-stx: actual-cost,
                performed-by: (some tx-sender),
                efficiency-impact: efficiency-impact,
                notes: notes
            })
        )

        ;; Update turbine data
        (let ((current-efficiency (get efficiency-rating turbine-data))
              (new-efficiency (if (>= efficiency-impact 0)
                                (+ current-efficiency (to-uint efficiency-impact))
                                (if (>= current-efficiency (to-uint (- efficiency-impact)))
                                  (- current-efficiency (to-uint (- efficiency-impact)))
                                  u0))))
            (map-set turbines
                { turbine-id: turbine-id }
                (merge turbine-data {
                    last-maintenance: stacks-block-height,
                    efficiency-rating: (if (> new-efficiency u100) u100 new-efficiency),
                    status: "active"
                })
            )
        )

        (ok true)
    )
)

;; ===================================
;; ENVIRONMENTAL MONITORING
;; ===================================

(define-public (record-environmental-metrics (total-carbon-offset uint)
                                           (wildlife-impact-score uint)
                                           (noise-level-db uint)
                                           (biodiversity-index uint))
    (let ((current-period (/ stacks-block-height u144)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (<= wildlife-impact-score u100) ERR_INVALID_ENERGY_DATA)
        (asserts! (<= biodiversity-index u100) ERR_INVALID_ENERGY_DATA)

        (map-set environmental-metrics
            { block-period: current-period }
            {
                total-carbon-offset: total-carbon-offset,
                wildlife-impact-score: wildlife-impact-score,
                noise-level-db: noise-level-db,
                land-use-efficiency: (/ total-carbon-offset (var-get total-turbines)),
                biodiversity-index: biodiversity-index
            }
        )
        (ok true)
    )
)

;; ===================================
;; EMERGENCY FUNCTIONS
;; ===================================

(define-public (set-system-status (active bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set system-active active)
        (var-set emergency-shutdown (not active))
        (ok active)
    )
)

;; ===================================
;; READ-ONLY FUNCTIONS
;; ===================================

(define-read-only (get-turbine-data (turbine-id uint))
    (map-get? turbines { turbine-id: turbine-id })
)

(define-read-only (get-energy-production-data (turbine-id uint) (block-period uint))
    (map-get? energy-production { turbine-id: turbine-id, block-period: block-period })
)

(define-read-only (get-current-period)
    (/ stacks-block-height u144)
)

(define-read-only (get-grid-status (block-period uint))
    (map-get? grid-integration { block-period: block-period })
)

(define-read-only (get-environmental-metrics (block-period uint))
    (map-get? environmental-metrics { block-period: block-period })
)

(define-read-only (get-maintenance-record (turbine-id uint) (maintenance-id uint))
    (map-get? maintenance-records { turbine-id: turbine-id, maintenance-id: maintenance-id })
)

(define-read-only (is-maintenance-due (turbine-id uint))
    (match (map-get? turbines { turbine-id: turbine-id })
        turbine-data (> (- stacks-block-height (get last-maintenance turbine-data))
                       (get maintenance-interval turbine-data))
        false
    )
)

(define-read-only (get-system-status)
    {
        total-turbines: (var-get total-turbines),
        current-block: stacks-block-height
    }
)



;; ===================================
;; contracts/community-governance.clar
;; Community ownership and democratic governance contract
;; ===================================

(define-constant GOVERNANCE_OWNER tx-sender)
(define-constant ERR_NOT_MEMBER (err u200))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u201))
(define-constant ERR_VOTING_CLOSED (err u202))
(define-constant ERR_ALREADY_VOTED (err u203))
(define-constant ERR_INSUFFICIENT_SHARES (err u204))
(define-constant ERR_INVALID_PROPOSAL (err u205))

;; Member ownership tracking
(define-map community-members
    { member: principal }
    {
        shares-owned: uint,
        join-block: uint,
        total-contributions: uint, ;; STX contributed
        voting-power: uint,
        energy-credits: uint,
        last-dividend-claim: uint,
        member-status: (string-ascii 20) ;; "active", "inactive", "pending"
    }
)

;; Governance proposals
(define-map proposals
    { proposal-id: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        proposal-type: (string-ascii 50), ;; "turbine-addition", "budget", "policy", "emergency"
        proposer: principal,
        creation-block: uint,
        voting-end-block: uint,
        execution-block: (optional uint),
        total-votes-for: uint,
        total-votes-against: uint,
        total-voting-power: uint,
        status: (string-ascii 20), ;; "active", "passed", "rejected", "executed"
        required-majority: uint ;; percentage needed to pass
    }
)

;; Individual votes on proposals
(define-map member-votes
    { member: principal, proposal-id: uint }
    {
        vote: bool, ;; true for "for", false for "against"
        voting-power-used: uint,
        vote-block: uint
    }
)

;; Dividend distribution tracking
(define-map dividend-periods
    { period-id: uint }
    {
        start-block: uint,
        end-block: uint,
        total-revenue: uint, ;; STX from energy sales
        total-shares: uint,
        dividend-per-share: uint,
        distribution-complete: bool
    }
)

;; Member dividend claims
(define-map dividend-claims
    { member: principal, period-id: uint }
    {
        amount-claimed: uint,
        claim-block: uint,
        claimed: bool
    }
)

;; Energy credit system for community consumption
(define-map energy-credits-ledger
    { member: principal, credit-period: uint }
    {
        credits-issued: uint,
        credits-used: uint,
        kwh-consumed: uint,
        cost-savings: uint
    }
)

;; Global governance state
(define-data-var total-members uint u0)
(define-data-var total-shares uint u0)
(define-data-var next-proposal-id uint u1)
(define-data-var next-dividend-period uint u1)
(define-data-var governance-active bool true)
(define-data-var minimum-proposal-shares uint u100) ;; minimum shares to create proposal

;; ===================================
;; MEMBERSHIP MANAGEMENT
;; ===================================

(define-public (join-cooperative (initial-contribution uint))
    (let (
        (current-shares (calculate-shares-from-contribution initial-contribution))
        (voting-power (calculate-voting-power current-shares))
    )
        (asserts! (> initial-contribution u0) ERR_INSUFFICIENT_SHARES)
        (asserts! (is-none (map-get? community-members { member: tx-sender })) (err u206)) ;; already member

        (map-set community-members
            { member: tx-sender }
            {
                shares-owned: current-shares,
                join-block: stacks-block-height,
                total-contributions: initial-contribution,
                voting-power: voting-power,
                energy-credits: u0,
                last-dividend-claim: u0,
                member-status: "active"
            }
        )

        (var-set total-members (+ (var-get total-members) u1))
        (var-set total-shares (+ (var-get total-shares) current-shares))

        (ok current-shares)
    )
)

(define-public (increase-ownership (additional-contribution uint))
    (let (
        (member-data (unwrap! (map-get? community-members { member: tx-sender }) ERR_NOT_MEMBER))
        (additional-shares (calculate-shares-from-contribution additional-contribution))
        (new-total-shares (+ (get shares-owned member-data) additional-shares))
        (new-voting-power (calculate-voting-power new-total-shares))
    )
        (asserts! (> additional-contribution u0) ERR_INSUFFICIENT_SHARES)

        (map-set community-members
            { member: tx-sender }
            (merge member-data {
                shares-owned: new-total-shares,
                total-contributions: (+ (get total-contributions member-data) additional-contribution),
                voting-power: new-voting-power
            })
        )

        (var-set total-shares (+ (var-get total-shares) additional-shares))
        (ok new-total-shares)
    )
)

;; ===================================
;; DEMOCRATIC GOVERNANCE
;; ===================================

(define-public (create-proposal (title (string-ascii 100))
                              (description (string-ascii 500))
                              (proposal-type (string-ascii 50))
                              (voting-duration uint)
                              (required-majority uint))
    (let (
        (member-data (unwrap! (map-get? community-members { member: tx-sender }) ERR_NOT_MEMBER))
        (proposal-id (var-get next-proposal-id))
    )
        (asserts! (>= (get shares-owned member-data) (var-get minimum-proposal-shares)) ERR_INSUFFICIENT_SHARES)
        (asserts! (is-eq (get member-status member-data) "active") ERR_NOT_MEMBER)
        (asserts! (and (>= required-majority u50) (<= required-majority u100)) ERR_INVALID_PROPOSAL)

        (map-set proposals
            { proposal-id: proposal-id }
            {
                title: title,
                description: description,
                proposal-type: proposal-type,
                proposer: tx-sender,
                creation-block: stacks-block-height,
                voting-end-block: (+ stacks-block-height voting-duration),
                execution-block: none,
                total-votes-for: u0,
                total-votes-against: u0,
                total-voting-power: u0,
                status: "active",
                required-majority: required-majority
            }
        )

        (var-set next-proposal-id (+ proposal-id u1))
        (ok proposal-id)
    )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
    (let (
        (member-data (unwrap! (map-get? community-members { member: tx-sender }) ERR_NOT_MEMBER))
        (proposal-data (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
        (member-voting-power (get voting-power member-data))
    )
        (asserts! (is-eq (get member-status member-data) "active") ERR_NOT_MEMBER)
        (asserts! (< stacks-block-height (get voting-end-block proposal-data)) ERR_VOTING_CLOSED)
        (asserts! (is-eq (get status proposal-data) "active") ERR_VOTING_CLOSED)
        (asserts! (is-none (map-get? member-votes { member: tx-sender, proposal-id: proposal-id })) ERR_ALREADY_VOTED)

        ;; Record the vote
        (map-set member-votes
            { member: tx-sender, proposal-id: proposal-id }
            {
                vote: vote-for,
                voting-power-used: member-voting-power,
                vote-block: stacks-block-height
            }
        )

        ;; Update proposal tallies
        (let (
            (new-votes-for (if vote-for
                             (+ (get total-votes-for proposal-data) member-voting-power)
                             (get total-votes-for proposal-data)))
            (new-votes-against (if vote-for
                                (get total-votes-against proposal-data)
                                (+ (get total-votes-against proposal-data) member-voting-power)))
            (new-total-voting-power (+ (get total-voting-power proposal-data) member-voting-power))
        )
            (map-set proposals
                { proposal-id: proposal-id }
                (merge proposal-data {
                    total-votes-for: new-votes-for,
                    total-votes-against: new-votes-against,
                    total-voting-power: new-total-voting-power
                })
            )
        )

        (ok true)
    )
)

(define-public (finalize-proposal (proposal-id uint))
    (let (
        (proposal-data (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
        (total-votes (+ (get total-votes-for proposal-data) (get total-votes-against proposal-data)))
        (approval-percentage (if (> total-votes u0)
                               (/ (* (get total-votes-for proposal-data) u100) total-votes)
                               u0))
    )
        (asserts! (>= stacks-block-height (get voting-end-block proposal-data)) (err u207))
        (asserts! (is-eq (get status proposal-data) "active") ERR_VOTING_CLOSED)

        (let ((new-status (if (>= approval-percentage (get required-majority proposal-data))
                            "passed"
                            "rejected")))
            (map-set proposals
                { proposal-id: proposal-id }
                (merge proposal-data { status: new-status })
            )
            (ok new-status)
        )
    )
)

;; ===================================
;; DIVIDEND DISTRIBUTION
;; ===================================

(define-public (create-dividend-period (total-revenue uint) (duration uint))
    (let (
        (period-id (var-get next-dividend-period))
        (current-total-shares (var-get total-shares))
        (dividend-per-share (/ total-revenue current-total-shares))
    )
        (asserts! (is-eq tx-sender GOVERNANCE_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> total-revenue u0) ERR_INVALID_PROPOSAL)
        (asserts! (> current-total-shares u0) ERR_INVALID_PROPOSAL)

        (map-set dividend-periods
            { period-id: period-id }
            {
                start-block: stacks-block-height,
                end-block: (+ stacks-block-height duration),
                total-revenue: total-revenue,
                total-shares: current-total-shares,
                dividend-per-share: dividend-per-share,
                distribution-complete: false
            }
        )

        (var-set next-dividend-period (+ period-id u1))
        (ok period-id)
    )
)

(define-public (claim-dividend (period-id uint))
    (let (
        (member-data (unwrap! (map-get? community-members { member: tx-sender }) ERR_NOT_MEMBER))
        (dividend-data (unwrap! (map-get? dividend-periods { period-id: period-id }) ERR_PROPOSAL_NOT_FOUND))
        (member-shares (get shares-owned member-data))
        (dividend-amount (* member-shares (get dividend-per-share dividend-data)))
    )
        (asserts! (is-eq (get member-status member-data) "active") ERR_NOT_MEMBER)
        (asserts! (is-none (map-get? dividend-claims { member: tx-sender, period-id: period-id })) ERR_ALREADY_VOTED)
        (asserts! (>= stacks-block-height (get start-block dividend-data)) (err u208))
        (asserts! (<= stacks-block-height (get end-block dividend-data)) (err u209))

        (map-set dividend-claims
            { member: tx-sender, period-id: period-id }
            {
                amount-claimed: dividend-amount,
                claim-block: stacks-block-height,
                claimed: true
            }
        )

        ;; Update member's last dividend claim
        (map-set community-members
            { member: tx-sender }
            (merge member-data { last-dividend-claim: period-id })
        )

        (ok dividend-amount)
    )
)

;; ===================================
;; ENERGY CREDITS SYSTEM
;; ===================================

(define-public (issue-energy-credits (member principal) (credits-amount uint) (kwh-basis uint))
    (let (
        (member-data (unwrap! (map-get? community-members { member: member }) ERR_NOT_MEMBER))
        (current-period (/ stacks-block-height u144))
        (cost-savings (* kwh-basis u15)) ;; Assume 15 STX savings per kWh
    )
        (asserts! (is-eq tx-sender GOVERNANCE_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> credits-amount u0) ERR_INVALID_PROPOSAL)

        ;; Update member's energy credits
        (map-set community-members
            { member: member }
            (merge member-data {
                energy-credits: (+ (get energy-credits member-data) credits-amount)
            })
        )

        ;; Record the credit transaction
        (map-set energy-credits-ledger
            { member: member, credit-period: current-period }
            {
                credits-issued: credits-amount,
                credits-used: u0,
                kwh-consumed: kwh-basis,
                cost-savings: cost-savings
            }
        )

        (ok true)
    )
)

;; ===================================
;; HELPER FUNCTIONS
;; ===================================

(define-private (calculate-shares-from-contribution (contribution uint))
    ;; Simple 1:1 ratio for now - 1 STX = 1 share
    contribution
)

(define-private (calculate-voting-power (shares uint))
    ;; Implement quadratic voting to prevent wealth concentration
    ;; voting power = sqrt(shares) * 10 for precision
    (let ((sqrt-shares (pow shares u1))) ;; Simplified - would use actual sqrt
        (let ((raw-power (* sqrt-shares u10)))
            (if (<= raw-power u1000) raw-power u1000) ;; Cap at 1000 voting power
        )
    )
)

;; ===================================
;; READ-ONLY FUNCTIONS
;; ===================================

(define-read-only (get-member-data (member principal))
    (map-get? community-members { member: member })
)

(define-read-only (get-proposal-data (proposal-id uint))
    (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-member-vote (member principal) (proposal-id uint))
    (map-get? member-votes { member: member, proposal-id: proposal-id })
)

(define-read-only (get-dividend-period (period-id uint))
    (map-get? dividend-periods { period-id: period-id })
)

(define-read-only (get-dividend-claim (member principal) (period-id uint))
    (map-get? dividend-claims { member: member, period-id: period-id })
)

(define-read-only (get-governance-stats)
    {
        total-members: (var-get total-members),
        total-shares: (var-get total-shares),
        governance-active: (var-get governance-active),
        minimum-proposal-shares: (var-get minimum-proposal-shares),
        current-block: stacks-block-height
    }
)

(define-read-only (calculate-proposal-result (proposal-id uint))
    (match (map-get? proposals { proposal-id: proposal-id })
        proposal-data
        (let (
            (total-votes (+ (get total-votes-for proposal-data) (get total-votes-against proposal-data)))
            (approval-percentage (if (> total-votes u0)
                                   (/ (* (get total-votes-for proposal-data) u100) total-votes)
                                   u0))
        )
            (some {
                approval-percentage: approval-percentage,
                total-votes: total-votes,
                votes-for: (get total-votes-for proposal-data),
                votes-against: (get total-votes-against proposal-data),
                would-pass: (>= approval-percentage (get required-majority proposal-data))
            })
        )
        none
    )
)
