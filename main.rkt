#lang racket
(require "WorkzoneTDA.rkt")
(require "remoteRepoTDA.rkt")
(require "HistoryTDA.rkt")


;GIT
;descripcion: Funcion git.
;dom: procedimiento X area de trabajo
;rec: procedimiento
(define git (lambda (function)
              (lambda (workzone)
                (function workzone))))


;ADD
;descripcion: Funcion envelope de add.
;dom: lista X area de trabajo
;rec: area de trabajo
(define add (lambda (files)
              (lambda (workzone)
                (set_history "add" (add_envelope files workzone)))))
		
		
;descripcion: Funcion add.
;dom: lista X area de trabajo
;rec: area de trabajo
;recursion: natural
(define (add_envelope files workzone)
  (if [null? files]
      workzone
      (if [belongs? (car files) (get_index workzone)]
          (add_envelope (cdr files) workzone)
          (add_envelope (cdr files)
                   (set_index (append (list (car files)) (get_index workzone)) workzone)))))
				

;COMMIT
;descripcion: Funcion envelope de commit.
;dom: string X area de trabajo
;rec: area de trabajo
(define commit (lambda (message)
                 (lambda (workzone)
                   (set_history (string-append "commit -m " message)
                                (commit_envelope message workzone)))))		

		
;descripcion: Funcion commit.
;dom: string X area de trabajo
;rec: area de trabajo
(define (commit_envelope message workzone)
  (set_index null
             (set_localRepo
              (append
               (list (string-append message " " (list-->string (get_index workzone))))
               (get_localRepo workzone))
              workzone)))


;PULL
;descripcion: Funcion envelope de pull.
;dom: area de trabajo
;rec: area de trabajo
(define (pull workzone)
  (set_history "pull"
               (set_workspace
                (pull_envelope (cdr (get_currentRemoteRepo workzone)) (get_workspace workzone) null)
                workzone)))


;descripcion: Funcion pull.
;dom: lista X repositorio remoto X lista
;rec: repositorio remoto
;recursion: cola
(define (pull_envelope remoteRepoCarry workspaceStatic ToBeAdded)
  (if [null? remoteRepoCarry]
      (append ToBeAdded workspaceStatic)
      (if [belongs? (car remoteRepoCarry) workspaceStatic]
          (pull_envelope (cdr remoteRepoCarry) workspaceStatic
                         ToBeAdded)
          (pull_envelope (cdr remoteRepoCarry) workspaceStatic
                         (append ToBeAdded (list (car remoteRepoCarry)))))))


;PUSH
;descripcion: Funcion push.
;dom: area de trabajo
;rec: area de trabajo
(define (push workzone)
  (set_history "push"
               (set_currentRemoteRepo
                (append (get_currentRemoteRepo workzone) (get_localRepo workzone)) workzone)))


;ZONAS->STRING
;descripcion: Funcion zonas->string.
;dom: area de trabajo
;rec: string
(define (zonas->string workzone)
  (string-append
   "History:\n"
   (list-->string (get_history workzone))
   "\n\nWorkspace:\n"
   (list-->string (get_workspace workzone))
   "\n\nIndex:\n"
   (list-->string (get_index workzone))
   "\n\nLocal Repository:\n"
   (list-->string (get_localRepo workzone))
   "\n\nRemote Repositories:"
   (remoteRepo->string (get_remoteRepoDirectory workzone) "")
  ))


;STATUS
;descripcion: Funcion status.
;dom: area de trabajo
;rec: string
(define (status workzone)
  (string-append
   "\nArchivos en Index\n"
   (list-->string (get_index workzone))
   "\nCommits en repositorio local: "
   (number->string (length (get_localRepo workzone)))
   "\nRama actual: "
   (get_currentBranch workzone)))


;LOG
;descripcion: Funcion log.
;dom: area de trabajo
;rec: string
(define (log workzone)
  (log_envelope (get_localRepo workzone) "" 5))


;descripcion: Funcion log (recursion de cola).
;dom: repositorio local X string
;rec: string
;recursion: cola
(define (log_envelope localRepo stringCarry counter)
  (if [= counter 0]
      stringCarry
   (if [null? localRepo]
      stringCarry
      (log_envelope
       (cdr localRepo)
       (string-append stringCarry "\n" (car localRepo))
       (- counter 1)))))


;BRANCH
;descripcion: Funcion envelope de branch.
;dom: string X area de trabajo
;rec: area de trabajo
(define branch (lambda (branchName)
                 (lambda (workzone)
                   (set_history
                    (string-append branchName "branch")
                    (branch_envelope branchName workzone)))))


;descripcion: Funcion branch.
;dom: string X area de trabajo
;rec: area de trabajo
(define (branch_envelope branchName workzone)
  (list
   (get_history workzone)
   (get_workspace workzone)
   (get_index workzone)
   (get_localRepo workzone)
   (append
    (get_remoteRepoDirectory workzone)
    (set_branchName (car (get_remoteRepoDirectory workzone)) branchName))))


;CHECKOUT
;descripcion: Funcion envelope de checkout
;dom: string X area de trabajo
;rec: area de trabajo
(define checkout (lambda (branchName)
                   (lambda (workzone)
                     (set_history "checkout" (set_remoteRepoDirectory
                      (checkout_envelope
                       (get_remoteRepoDirectory workzone)
                       (get_branch branchName (get_remoteRepoDirectory workzone))
                       (list (get_branch branchName (get_remoteRepoDirectory workzone))))
                      workzone)))))


;descripcion: Funcion checkout
;dom: lista X lista X lista
;rec: lista
(define (checkout_envelope remoteRepoDirectory firstRepo finalDirectory)
  (if [null? remoteRepoDirectory]
      finalDirectory
      (if [equal? (car remoteRepoDirectory) firstRepo]
          (checkout_envelope (cdr remoteRepoDirectory) firstRepo finalDirectory)
          (checkout_envelope (cdr remoteRepoDirectory) firstRepo
                             (append finalDirectory (list (car remoteRepoDirectory)))))))



;;EJEMPLOS
;((git pull) test1)
;((git pull) test2)
;((git pull) test3)
;((git push) test1)
;((git push) test2)
;((git push) test3)
;(((git add) (list "pikachu.txt" "silla.rkt")) test1)
;(((git add) (list "pikachu.txt" "silla.rkt")) test2)
;(((git add) null) test3)
;(((git commit) "primer commit") test1)
;(((git commit) "segundo commit") test2)
;(((git commit) "tercer commit") test3)
;(zonas->string (((git add) (list "pikachu.txt" "silla.rkt")) test2))
;(zonas->string (((git commit) "tercer commit") test3))
;(zonas->string ((git pull) test3))
;((git status) test1)
;((git status) ((git push) test1))
;((git status) test3)
;((git log) (((git commit) "segundo commit") test2))
;((git log) test3)
;((git log) test1)

(define test1
  (list
   null
   null
   (list "file1.rkt" "file2.rkt" "file3.rkt" "file4.rkt")
   (list "ignore .gitignore" "initialization readme.md")
   (list (list "master" "initialization readme.md")))
  )

(define test2
  (list
   (list "0 git init" "1 gitignore")
   (list "file5.rkt" "file6.rkt" "file7.rkt" "file8.rkt" )
   (list "file1.rkt" "file2.rkt" "file3.rkt" "file4.rkt" )
   (list "ignore .gitignore" "initialization readme.md")
   (list (list "master" "initialization readme.md") (list "branch"  "initialization2 readme.md")))
  )

(define test3
  (list
   (list "0 git init" "1 gitignore")
   (list "file5.rkt" "file6.rkt" "file7.rkt" "file8.rkt" "image.png" )
   (list "file1.rkt")
   (list "ignore .gitignore" "initialization readme.md" "imaging image.png")
   (list (list "branch3" "initialization readme.md")
         (list "master"  "initialization2 readme.md")
         (list "branch"  "initialization2 readme.md")))
  )