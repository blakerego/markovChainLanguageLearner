;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Blake Jon Rego
; Artificial Intelligence
; HW6 - One Step Markov Chain Language Identifiers


;; Global variable myLanguageKnowledge
;; This is a list that will contain all of the probability matrices. 
;; It will have the following form: 
;; ( ("languageName1" probMat1) ("langName2" probMat2) ... ("langNameN probMatN) )

(setq myLanguageKnowledge nil)
(setq probabilityList nil)
(setf *print-array* t)             ;Allows us to print our matrices (for debuggin purposes)


;;;;;;;;;;;;;;;;;;;; Top Level Functions:  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun learn (language) 
  (setq myLanguageKnowledge 
	(append
	 myLanguageKnowledge
	 (list (list language (createProbabilityTable (eliminateZeroes 
						       (createFrequencyTable language)
						       ))))))
  (format nil (concatenate 'string "I now know " (string language)))
  )




;;;;;;;;;;;;;;;;;;;; All other functions: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


       ;;;;;;;;;; Learning:

(defun createFrequencyTable (filename)
  (setq freqArray (make-array '(27 27)))
  (setq stream (open (string filename) :direction :input))
  (setq lastChar #\.)                               ;Assume the file starts with a period
  (do ((char (read-char stream nil)                 ;Reading input file
	     (read-char stream nil)))               
      ((null char))                                 ;End Condition
    
    ;;Update frequency table: 
    (if (null (aref freqArray (getPos lastChar) (getPos char)))    
	(setf (aref freqArray (getPos lastChar) (getPos char)) 1)
	(setf (aref freqArray (getPos lastChar) (getPos char)) 
	      (+ 1 (aref freqArray (getPos lastChar) (getPos char)))))
    
    (setq lastChar char)                            ;Current char is now lastChar
    )                                               ;End do block  
  freqArray                                         ;Return freqency matrix
  )

(defun getPos (inputChar)
  (cond
    ;;Lower case letters:
    ( (and (>= (char-code inputChar) 97) (<= (char-code inputChar) 122)) 
     (- (char-code inputChar) 96))
    ( (and (>= (char-code inputChar) 65) (<= (char-code inputChar) 90))
     (- (char-code inputChar) 64))
    ( t 0 )))


(defun eliminateZeroes (freqArray)  
  (loop for i from 0 to 26 do 
       (loop for j from 0 to 26 do
	    (if (null (aref freqArray i j))
		(setf (aref freqArray i j) 1/10)
		)))  
  freqArray)

(defun createProbabilityTable (freqTable)
  (loop for prevLetter from 0 to 26 do
       (setq thisNorm (getNormalization freqTable prevLetter))
       (loop for j from 0 to 26 do
	    (setf (aref freqTable prevLetter j) 
		  (/ (aref freqTable prevLetter j) thisNorm))
	    )
       )
  freqTable
  )

(defun getNormalization (freqTable prevCharNumber &optional (currentSum 0))
  (loop for i from 0 to 26 do
       (setq currentSum (+ currentSum (aref  freqArray prevCharNumber i))))
  currentSum
  )

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Testing Side    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST is a top level function that will use knowledge learned to try to identify 
;; individual lines from a file. 
;;
;; input: name of a test file in string form
;; output: prints out most likely language for each line along with the associated prob. 
;;
;; This function works in 3 nested parts: 
;; (1) Iterate through each line of the file
;; (2) For each line of the file, try each language known. 
;; (3) For each language, use each character to get a probability value
;; (4) Compare probabilities from languages against each other to get final 
;; relative probability. 

(defun test (testfile) 
  
  ;; Open up test file: 
  (let ( (stream (open (string testfile) :direction :input)) (lastChar #\.) (myProb 1))
    (declare special lastChar myProb)
    
    ;; Iterate through each line
    (do ( (line (read-line stream nil) 
		(read-line stream nil))) 
	((null line))                                
      
      (if (not (null line))                                   ;Don't read a blank line
	  (progn
	    (let ( (probabilityList nil) )
	      (declare probabilityList special)
	      
	      (format t "~A: ~%" line)                          ;Print the line for reference
	      (cycleThroughLanguages myLanguageKnowledge line)  ;Within each line, cycle through each language
	      )
	    
	    
            
	    (let ( (probabilitySum (probSum probabilityList)) )     ;Sum all probabilities to aid relative prob.
	      (setf best '("initial" 0))                            ;Initialize best outside recursive function.
	      (compareProbabilities probabilityList probabilitySum) ;This will find the best fit. 
	      (format t "This line is most likely ~A ~%" (string (car best))) ;Print results to screen
	      (format t "The probability of this is ~A ~3%" (float (/ (cadr best) probabilitySum)) )
	      )
	  
	    (setq probabilityList nil)                              ;Reset probabilityList for next line. 
	    )))))
	  

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cycleThroughLanguages is a recursive function that tries each language known so far. 
;; For this function to work properly, it requires a global variable called probabilityList
;; to be set to nil before this function is called. The function operates on this assumption.

(defun cycleThroughLanguages (languageList line)
  
  (if (null (cdr languageList))                  ;Base Case
      (setq probabilityList (append probabilityList (list (list (caar languageList) (cycleThroughCharacters (cadar languageList) line) ))))
      (progn                                     ;Recursively check next element until we reach base.
	(setq probabilityList (append probabilityList (list (list (caar languageList) (cycleThroughCharacters (cadar languageList) line) ) )))
	(cycleThroughLanguages (cdr languageList) line)	
	)
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cycleThroughCharacters 
;; returns the probability that a given line comes from the language of the probability matrix
;; 
;; Input: 
;;   probMat - probability Matrix in the form of a 27 x 27 array
;;   line - String in question
;;   initial probability that is set to 1
;;   lastChar is initialized as a '.'

(defun cycleThroughCharacters (probMat line &optional (myProb 1) (lastChar #\.))
  (loop for char from 0 to (- (length line) 1) do
       (setq myProb (* myProb (aref probMat (getPos lastChar) (getPos (char line char)))))       
       (setq lastChar (char line char)) 
       )
  myProb
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; probSum
;; simple helper function that gets the sum of second elements in a superlist. For example:
;; ( (a 2) (b 3) (c 4) (d 8) ) will yield 17 when called by this function. 
;; input: 
;;  superList - Requires numbers to be in the second element of each sub-list. 
;;  optional variable sum - used in recursive calls. 

(defun probSum (superList &optional (sum 0) )
  (if (null (cdr superList))                      ; Base Case
      (+ sum (cadar superList))
      (progn                                      ; Recursively move on to the next element. 
	(setq sum (+ sum (cadar superList)))
	(probSum (cdr superList) sum)
	)
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compareProbabilities
;; This function prints all of our results to the screen. It checks our superlist of probabilities
;; for each line, compares them against each other, and chooses the most likely language. 
;;
;; input: 
;;   superList --> ( ("langName1" probability1) ("langName2" probability2) ... ("langNameN" probabilityN))
;;   sum --> sum of all probability values. This will be used to normalize probabilities.

(defun compareProbabilities (superList sum)

;; Uncomment the following two lines to see the probability for all laguages: 
;  (format t "The probability that this line is ~A is ~A. ~%"
;	  (string (caar superList)) (float (/ (cadar superList) sum)))
	
  ;;Base Case: 
  (if (null (cdr superList))
      (if (> (cadar superList) (cadr best))
	  (setq best (car superList))
	  )
      
      ;;All other cases:
      (if (> (cadar superList) (cadr best))
	  (progn 
	    (setq best (car superList))
	    (compareProbabilities (cdr superList) sum)
	    )
	  (compareProbabilities (cdr superList) sum)
	  )
      )

  )

