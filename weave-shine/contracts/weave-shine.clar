;; WeavShine - Dynamic NFT Gaming Ecosystem
;; A revolutionary blockchain gaming platform with evolving NFT characters

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-character-not-found (err u103))
(define-constant err-invalid-trait (err u104))
(define-constant err-cooldown-active (err u105))
(define-constant err-insufficient-governance-power (err u106))

;; Data Variables
(define-data-var last-character-id uint u0)
(define-data-var total-weave-supply uint u0)
(define-data-var treasury-balance uint u0)
(define-data-var platform-fee-percentage uint u5) ;; 5% platform fee

;; Character NFT Structure
(define-map characters
  uint
  {
    owner: principal,
    name: (string-ascii 50),
    level: uint,
    experience: uint,
    skill-rating: uint,
    creation-block: uint,
    last-activity: uint,
    evolution-stage: uint,
    backstory-hash: (string-ascii 64)
  }
)

;; Character Traits (Evolutionary Assets)
(define-map character-traits
  {character-id: uint, trait-name: (string-ascii 30)}
  {
    trait-value: uint,
    unlocked-at-block: uint,
    trait-tier: uint
  }
)

;; WEAVE Token Balances
(define-map weave-balances
  principal
  uint
)

;; Governance Power (Token Holdings + Skill Rating)
(define-map governance-power
  principal
  {
    token-weight: uint,
    skill-weight: uint,
    total-votes-cast: uint
  }
)

;; Contribution Tracking (Proof of Contribution)
(define-map player-contributions
  principal
  {
    gameplay-score: uint,
    community-events: uint,
    creator-contributions: uint,
    last-reward-block: uint
  }
)

;; Governance Proposals
(define-map proposals
  uint
  {
    proposer: principal,
    description: (string-ascii 256),
    yes-votes: uint,
    no-votes: uint,
    status: (string-ascii 20),
    created-at: uint,
    ends-at: uint
  }
)

(define-data-var last-proposal-id uint u0)

;; Read-only functions

(define-read-only (get-character (character-id uint))
  (map-get? characters character-id)
)

(define-read-only (get-character-trait (character-id uint) (trait-name (string-ascii 30)))
  (map-get? character-traits {character-id: character-id, trait-name: trait-name})
)

(define-read-only (get-weave-balance (account principal))
  (default-to u0 (map-get? weave-balances account))
)

(define-read-only (get-total-supply)
  (var-get total-weave-supply)
)

(define-read-only (get-governance-power-data (account principal))
  (map-get? governance-power account)
)

(define-read-only (get-contribution-data (account principal))
  (map-get? player-contributions account)
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

;; Private functions

(define-private (calculate-experience-for-level (level uint))
  (* level (* level u100))
)

(define-private (update-governance-power (account principal) (tokens uint) (skill uint))
  (map-set governance-power
    account
    {
      token-weight: tokens,
      skill-weight: skill,
      total-votes-cast: (default-to u0 (get total-votes-cast (map-get? governance-power account)))
    }
  )
)

;; Public functions

;; Mint a new character NFT
(define-public (mint-character (name (string-ascii 50)))
  (let
    (
      (new-id (+ (var-get last-character-id) u1))
      (sender tx-sender)
    )
    (map-set characters
      new-id
      {
        owner: sender,
        name: name,
        level: u1,
        experience: u0,
        skill-rating: u100,
        creation-block: block-height,
        last-activity: block-height,
        evolution-stage: u1,
        backstory-hash: ""
      }
    )
    (var-set last-character-id new-id)
    (ok new-id)
  )
)

;; Add experience to character through gameplay
(define-public (add-experience (character-id uint) (exp-gained uint))
  (let
    (
      (character (unwrap! (map-get? characters character-id) err-character-not-found))
      (sender tx-sender)
      (new-exp (+ (get experience character) exp-gained))
      (current-level (get level character))
      (exp-needed (calculate-experience-for-level current-level))
      (new-level (if (>= new-exp exp-needed) (+ current-level u1) current-level))
    )
    (asserts! (is-eq sender (get owner character)) err-not-token-owner)
    (map-set characters
      character-id
      (merge character {
        experience: new-exp,
        level: new-level,
        last-activity: block-height
      })
    )
    (ok new-level)
  )
)

;; Unlock character trait (Evolutionary Asset Protocol)
(define-public (unlock-trait (character-id uint) (trait-name (string-ascii 30)) (trait-value uint))
  (let
    (
      (character (unwrap! (map-get? characters character-id) err-character-not-found))
      (sender tx-sender)
    )
    (asserts! (is-eq sender (get owner character)) err-not-token-owner)
    (asserts! (>= (get level character) u5) err-invalid-trait)
    (map-set character-traits
      {character-id: character-id, trait-name: trait-name}
      {
        trait-value: trait-value,
        unlocked-at-block: block-height,
        trait-tier: (/ (get level character) u5)
      }
    )
    (ok true)
  )
)

;; Mint WEAVE tokens (only contract owner for initial distribution)
(define-public (mint-weave (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set weave-balances
      recipient
      (+ (get-weave-balance recipient) amount)
    )
    (var-set total-weave-supply (+ (var-get total-weave-supply) amount))
    (ok true)
  )
)

;; Transfer WEAVE tokens
(define-public (transfer-weave (amount uint) (recipient principal))
  (let
    (
      (sender tx-sender)
      (sender-balance (get-weave-balance sender))
    )
    (asserts! (>= sender-balance amount) err-insufficient-balance)
    (map-set weave-balances sender (- sender-balance amount))
    (map-set weave-balances recipient (+ (get-weave-balance recipient) amount))
    (ok true)
  )
)

;; Reward contribution (Proof of Contribution mechanism)
(define-public (reward-contribution (player principal) (gameplay-points uint) (community-points uint) (creator-points uint))
  (let
    (
      (current-data (default-to 
        {gameplay-score: u0, community-events: u0, creator-contributions: u0, last-reward-block: u0}
        (map-get? player-contributions player)
      ))
      (total-contribution (+ (+ gameplay-points community-points) creator-points))
      (reward-amount (* total-contribution u10)) ;; 10 WEAVE per contribution point
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set player-contributions
      player
      {
        gameplay-score: (+ (get gameplay-score current-data) gameplay-points),
        community-events: (+ (get community-events current-data) community-points),
        creator-contributions: (+ (get creator-contributions current-data) creator-points),
        last-reward-block: block-height
      }
    )
    ;; Mint reward tokens
    (map-set weave-balances player (+ (get-weave-balance player) reward-amount))
    (var-set total-weave-supply (+ (var-get total-weave-supply) reward-amount))
    (ok reward-amount)
  )
)

;; Create governance proposal (Quadratic Voting)
(define-public (create-proposal (description (string-ascii 256)))
  (let
    (
      (new-proposal-id (+ (var-get last-proposal-id) u1))
      (sender tx-sender)
      (gov-power (default-to {token-weight: u0, skill-weight: u0, total-votes-cast: u0} 
                              (map-get? governance-power sender)))
      (total-power (+ (get token-weight gov-power) (get skill-weight gov-power)))
    )
    (asserts! (>= total-power u100) err-insufficient-governance-power)
    (map-set proposals
      new-proposal-id
      {
        proposer: sender,
        description: description,
        yes-votes: u0,
        no-votes: u0,
        status: "active",
        created-at: block-height,
        ends-at: (+ block-height u1440) ;; ~10 days
      }
    )
    (var-set last-proposal-id new-proposal-id)
    (ok new-proposal-id)
  )
)

;; Vote on proposal (Quadratic Voting System)
(define-public (vote-on-proposal (proposal-id uint) (vote-yes bool))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) err-character-not-found))
      (sender tx-sender)
      (gov-power (default-to {token-weight: u0, skill-weight: u0, total-votes-cast: u0}
                              (map-get? governance-power sender)))
      (voting-power (+ (get token-weight gov-power) (get skill-weight gov-power)))
      (quadratic-votes (sqrti voting-power)) ;; Quadratic voting to prevent whale dominance
    )
    (asserts! (is-eq (get status proposal) "active") err-cooldown-active)
    (asserts! (<= block-height (get ends-at proposal)) err-cooldown-active)
    (map-set proposals
      proposal-id
      (merge proposal {
        yes-votes: (if vote-yes (+ (get yes-votes proposal) quadratic-votes) (get yes-votes proposal)),
        no-votes: (if vote-yes (get no-votes proposal) (+ (get no-votes proposal) quadratic-votes))
      })
    )
    (ok quadratic-votes)
  )
)

;; Update character backstory hash
(define-public (update-backstory (character-id uint) (backstory-hash (string-ascii 64)))
  (let
    (
      (character (unwrap! (map-get? characters character-id) err-character-not-found))
      (sender tx-sender)
    )
    (asserts! (is-eq sender (get owner character)) err-not-token-owner)
    (map-set characters
      character-id
      (merge character {backstory-hash: backstory-hash})
    )
    (ok true)
  )
)

;; Evolve character to next stage
(define-public (evolve-character (character-id uint))
  (let
    (
      (character (unwrap! (map-get? characters character-id) err-character-not-found))
      (sender tx-sender)
      (current-stage (get evolution-stage character))
    )
    (asserts! (is-eq sender (get owner character)) err-not-token-owner)
    (asserts! (>= (get level character) (* current-stage u10)) err-invalid-trait)
    (map-set characters
      character-id
      (merge character {
        evolution-stage: (+ current-stage u1),
        last-activity: block-height
      })
    )
    (ok (+ current-stage u1))
  )
)

;; Update skill rating based on performance
(define-public (update-skill-rating (character-id uint) (new-rating uint))
  (let
    (
      (character (unwrap! (map-get? characters character-id) err-character-not-found))
      (sender tx-sender)
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set characters
      character-id
      (merge character {skill-rating: new-rating})
    )
    (update-governance-power (get owner character) (get-weave-balance (get owner character)) new-rating)
    (ok true)
  )
)