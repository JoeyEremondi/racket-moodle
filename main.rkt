#lang racket

(require net/http-easy)
(require json)

;; Base URL of the moodle server
(define current-base-url (make-parameter "urcourses.uregina.ca"))

;; Location of a text file containing your private Moodle access token
(define current-token (make-parameter (file->string "token.txt")))

;;Symbol for what field is used to map users to their Moodle IDs
;; e.g. if their handin username us their email address, this should be 'email
(define current-moodle-ident-field (make-parameter 'email))

;; Moodle Course ID for the course you're dealing with
(define current-courseid (make-parameter "33282"))

;; Post the given moodle function name to the server,
;; with a jsonexp? for parameters
(define (urPost function params #:json [json '()])
  (response-json
   (post (string-append (current-base-url) "/webservice/rest/server.php")
         #:params
         `((moodlewsrestformat . "json")
           (wsfunction . ,function)
           (moodlewssettingfilter . "true")
           (moodlewssettingfileurl . "true")
           (wstoken . ,(current-token))
           . ,params)
         #:json json)))

;; Post the given moodle function name to the server,
;; in the context of the current course,
;; with a jsonexp? for parameters
(define (urPostCourse function params #:json [json '()])
  (response-json
   (post (string-append (current-base-url) "/webservice/rest/server.php")
         #:params
         `((moodlewsrestformat . "json")
           (wsfunction . ,function)
           (moodlewssettingfilter . "true")
           (moodlewssettingfileurl . "true")
           (wstoken . ,(current-token))
           (courseid . ,(current-courseid))
           . ,params)
         #:json json)))



(define (get-assignments)
  (urPost
   "mod_assign_get_assignments"
   '()
   #:json (hasheq 'courseids  (list (current-courseid)))))


;; Mapping from email addresses to moodle user ID numbers.
;; Initially empty, populated by queryiung the server.
;; The parameter (current-moodle-ident-field)
;; determines which field of the user's profile is used to identify them, e.g. as their handin id.
;; By default it's their email address.

;; Hashof Email MoodleIdNum
(define current-user-ids
  (make-hash))

;; make-user-list : -> Hashof StudentNumber MoodleId
;; Query the moodle server to get the list of users in the current course
(define (make-id-dict)
  (define ulist (urPostCourse
                 "core_enrol_get_enrolled_users"
                 '()))
  (for ([rec ulist])
    (hash-set! current-user-ids (hash-ref rec (current-moodle-ident-field))
               (number->string (hash-ref rec 'id)))))

;; -> Ident MoodleIDNum
;; Map a student's hanin username to their moodle ID number,
;; querying the server if it's not found.
(define (ident->moodleId ident)
  ;; If the ID isn't found, then update the list of student IDs from the server
  (when (not (hash-has-key? current-user-ids ident))
    (make-id-dict))
  ;; Regardless, get the id
  ;; Something is broken if this fails
  (hash-ref current-user-ids ident))

;; Upload the feedback file for the given user
(define (upload-grade userIdent assignId gradeValue feedbackStr)
  (urPost "mod_assign_save_grade"
          `((assignmentid . ,assignId)
            (userid . ,(ident->moodleId userIdent))
            (grade . ,(number->string gradeValue))
            (attemptnumber . "-1")
            (addattempt . "0")
            (workflowstate . "TODO")
            (applytoall . "0")
            )
          #:json (hasheq 'plugindata (hasheq 'assignfeedbackcomments_editor
                                             (hasheq 'text feedbackStr 'format 1)) )))

