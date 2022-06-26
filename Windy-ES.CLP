;function that is passed to other functions that are used for asking questions
;is used for input validation and code reusability
;checks for string input and converts it to lowercase
(deffunction ask-question (?question $?allowed-values)
   (printout t "Q: " ?question " [" (implode$ ?allowed-values) "]: " )
   (bind ?answer (read))
   (if (lexemep ?answer)
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t "Q: " ?question " [" (implode$ ?allowed-values) "]: ")
      (bind ?answer (read))
      (if (lexemep ?answer)
          then (bind ?answer (lowcase ?answer))))
   ?answer)

;function that checks for integer input
(deffunction ask-number (?question)
   (printout t "Q: " ?question)
   (bind ?answer (read))
   (if (integerp ?answer)
       then (bind ?answer (integer ?answer)))
   (while (not (integerp ?answer)) do
      (printout t "Q: " ?question)
      (bind ?answer (read))
      (if (integerp ?answer)
          then (bind ?answer (integer ?answer))))
   ?answer)

(deffunction yes-or-no (?question)
   (ask-question ?question yes no))

(deffunction energy-production-answer (?question)
	(ask-question ?question low medium high))

(deffunction wildlife-score-answer (?question)
	(ask-number ?question))

(deffunction visual-impact-answer (?question ?score)
		(ask-question ?question close fairly-distant very-distant))

(deffunction sound-impact-answer (?question)
	(ask-question ?question quiet loud))

(deffunction geological-survey-answer (?question)
	(ask-question ?question stable partly-stable unstable))

;Test 1 - Check the hydrological survey
;Rejected if either are true
(defrule ask-hydrological-survey (not (is-hydrological-survey ?))
	=>
	(bind ?input (yes-or-no "Is there risk of erosion?"))
	(assert (is-erosion ?input))
	(bind ?input (yes-or-no "Is there risk of significant run-off or flooding?"))
	(assert (is-flooding ?input)))

(defrule reject-erosion (is-erosion yes)
	=>
	(printout t "Reject - completely unsuitable due to risk of erosion" crlf))

(defrule reject-flooding (is-flooding yes)
	=>
	(printout t "Reject - completely unsuitable due to risk of flooding" crlf))

;Test 2 - Check the energy production
(defrule ask-energy-production (and (is-erosion no) (is-flooding no))
	=>
	(bind ?input (energy-production-answer "What is the profitability of the energy production survey?"))
	(assert (is-energy-production ?input))
	(if (eq ?input medium)
		then
		(assert (site-quality second-best))))

(defrule reject-energy-production (is-energy-production low)
	=>
	(printout t "Reject - completely unsuitable due to not being profitable" crlf))

;Test 3 - Check the wildlife score
(defrule ask-wildlife-score (or (is-energy-production high) (is-energy-production medium))
	=>
	(printout t "Bird Scarcity Wildlife Diversity Test:" crlf)
	(bind ?number-endangered (wildlife-score-answer "Enter the number of distinct species for Endangered: "))
	(bind ?answer-endangered (* ?number-endangered 3))
	(bind ?number-scarce (wildlife-score-answer "Enter the number of distinct species for Scarce: "))
	(bind ?answer-scarce (* ?number-scarce 2))
	(bind ?number-common (wildlife-score-answer "Enter the number of distinct species for Common: "))
	(bind ?answer-common (* ?number-common 1))
	(assert (bird-score (+ ?answer-endangered ?answer-scarce ?answer-common))))

;if score greater than or equal to 20
(defrule reject-wildlife-score
	(bird-score ?score)
	(test (>= ?score 20))
	=>
	(printout t "Reject - completely unsuitable due to risk of harming the local wildlife" crlf))

;Test 4 - Check for visual impact
(defrule ask-visual-impact
	(bird-score ?score)
	=>
	(if (<= ?score 10)
		then
		(bind ?input (visual-impact-answer "How close is the location to inhabited areas?" ?score))
		(assert (is-visual-impact ?input))
		(if (or (eq ?input fairly-distant) (eq ?input close))
			then
			(assert (site-quality second-best))))
	;if score is higher than 10 or lower than 20, assert the site as second-best quality location
	(if (and (> ?score 10) (< ?score 20))
		then
		(bind ?input (visual-impact-answer "How close is the location to inhabited areas?" ?score))
		(assert (is-visual-impact ?input) (site-quality second-best))))

;Test 5 - Check the Sound impact
;Only asks for sound impact if site is close to inhabited area
(defrule ask-sound-impact (is-visual-impact close)
	=>
	(bind ?input (sound-impact-answer "Will the noise of the turbines reach impacted inhabited areas?"))
	(assert (is-sound-impact ?input)))

(defrule reject-sound-impact (is-sound-impact loud)
	=>
	(printout t "Reject - completely unsuitable due to sound impact on population" crlf))

;Test 6 - Check the geological survey
(defrule ask-geological-survey
	(or (is-visual-impact fairly-distant)
		(is-visual-impact very-distant)
		(is-sound-impact quiet))
	=>
	(bind ?input (geological-survey-answer "Is the ground sufficiently stable to support a heavy turbine?"))
	(assert (is-geological-survey ?input))
	(if (eq ?input partly-stable)
		then
		(assert (site-quality second-best))))

(defrule reject-geological-survey (is-geological-survey unstable)
	=>
	(printout t "Reject - completely unsuitable due to unstable ground" crlf))

(defrule ideal-location
	(bird-score ?score)
	(and (is-energy-production high)
		(not (site-quality second-best))
		(test (<= ?score 10))
		(is-visual-impact very-distant)
		(is-geological-survey stable))
	=>
	(printout t "Accept - an ideal location. Start building now." crlf))

(defrule second-best-location
	(bird-score ?score)
	(site-quality second-best)
	(or (is-geological-survey stable) (is-geological-survey partly-stable))
	=>
	(if (and (> ?score 10) (< ?score 20))
		then
		(printout t "Accept as second best quality location - stabilise the land and create a wildlife habitat if necessary." crlf)
		(assert (second-best-site accepted))
	else
	(printout t "Accept as second best quality location - stabilise the land if necessary." crlf))
	(assert (second-best-site accepted)))

(defrule energy-production-alert
	(and (is-energy-production medium) (second-best-site accepted))
	=>
	(printout t "Please consider that the site will not a high level of energy production." crlf))