type rank = Number of int | Jack | Queen | King | Ace
type suit = Hearts | Spades | Diamonds | Clubs
type card = {suit : suit; rank : rank}

val max_card : card option array -> suit -> suit -> int
val full_deck : card array