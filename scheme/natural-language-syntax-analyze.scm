(define nouns '(noun student professor cat class))

(define verbs '(verb studies lectures eats sleeps))

(define articlees '(article the a))

(define (parse-sentence)
  (list 'sentence
	(parse-noun-phrase)
	(parse-word verbs)))

(define (parse-noun-phrase)
  (list 'noun-phrase
	(parse-word articles)
	(parse-word nouns)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))
