;=======================;
;Author: Samantha Dobesh;
;June 1st 2020          ;
;Desc: Grammar Evaluator;
;=======================;==============================================================
;
;This program implements grammar tuples as well as
;functions and predicates to manipulate the grammars.

#lang racket

;====================
;Grammar Definitions:
;======================================================================================

;G1
(define G1 '((S)                           ;variables non term
             (a)                           ;alphabet  terminal
             (((S) (a S) ()))              ;rules
             S))                           ;start symbol

;G2
(define G2 '((S A)                         ;variables non term
             (a b c)                       ;alphabet  terminal
             (((S) (a A) ())               ;rules
              ((a A) (b S) (c)))           ;
             S))                           ;start symbol

;G3
(define G3 '((S A B C)                     ;variables non term
             (a b c d e f)                 ;alphabet  terminal
             (((S) (A a b c B d e f C) ()) ;rules
              ((A) (a B) (e C) (a))        ;
              ((B) (b C) (d))              ;
              ((C) (d) ()))                ;
             S))                           ;start symbol


;G4 Right regular
(define G4 '((S A B C)                     ;variables non term
             (a b c d e f)                 ;alphabet  terminal
             (((S) (a A))                  ;rules
              ((A) (a B) (e C) (a))        ;
              ((B) (b C) (d))              ;
              ((C) (d) ()))                ;
             S))                           ;start symbol


;G5 Left regular
(define G5 '((S A B C)                     ;variables non term
             (a b c d e f)                 ;alphabet  terminal
             (((S) (A b))                  ;rules
              ((A) (B a) (C e) (a))        ;
              ((B) (C b) (d))              ;
              ((C) (d) ()))                ;
             S))                           ;start symbol


;G6 Non context free
(define G6 '((S A B C)                     ;variables non term
             (a b c d e f)                 ;alphabet  terminal
             (((S) (A b))                  ;rules
              ((A a) (B a) (C e) (a))      ;
              ((B) (C b) (d))              ;
              ((a C) (d) ()))              ;
             S))                           ;start symbol


;G7 Informal Grammar:
;Contains a start with undefined symbol
(define G7 '((S A B C)                     ;variables non term
             (a b c d e f)                 ;alphabet  terminal
             (((S) (A b))                  ;rules
              ((A a) (B a) (C e) (a))      ;
              ((C) (C b) (d))              ;
              ((a C) (d) ()))              ;
             Z))                           ;start symbol


;G8 Informal Grammar:
;Contains variables in alphabet
(define G8 '((S A B C)                     ;variables non term
             (a S b C c d e f)             ;alphabet  terminal
             (((S) (A b))                  ;rules
              ((A a) (B a) (C e) (a))      ;
              ((C) (C b) (d))              ;
              ((a C) (d) ()))              ;
             Z))                           ;start symbol

;G9 Long Strings:
(define G9 '((S A B C D)                   ;variables non term
             (a b c d e f g h)             ;alphabet  terminal
             (((S) (A A a) (B f))        ;rules
              ((A) (A a) (C c) (B b) ())  ;
              ((C) (C c D) (a A) (D d) ())  ;
              ((B) (a d) ())               ;
              ((D) (C c) (A a) (b C c) ()));
             S))                           ;start symbol


;=============
;Set functions
;======================================================================================

;=====
;s-in?
;=====
;
;Desc: Returns true if element 'a' exists within set 'A'
;
;Parameters:
;    a (element): Element we are looking for
;    A (list): Set we are searching through
;
;Returns:
;    Boolean

(define (s-in? a A)
  
  ;Use or map to apply the equal? function
  ;to all of our list items.
  (ormap (lambda (x)
           
           ;due to ormap, if one element is true, then it returns true.
           (equal? a x))
         A)) 



;===========
;s-intersect
;===========
;
;Desc: This function takes two sets, and outputs a new set of the intersection
;('A' && 'B')
;
;Parameters:
;    A (list): Set 1 to be 'and'ed
;    B (list): Set 2 to be 'and'ed
;
;Returns:
;    a new set containing all and only the elements of the intersection of 'A' and 'B'
;    That is, only elements present in both sets

(define s-intersect (lambda (A B)

    ;First check for empty lists
    (cond ((empty? A) '())
          ((empty? B) '())

          ;Otherwise recursively cons together elements in both sets
          ((s-in? (car A) B)(cons (car A)(s-intersect (cdr A) B)))
          (else (s-intersect (cdr A) B)))))



;=======
;s-union
;=======
;
;Desc: This function combines 'A' and 'B' into one list, and then converts it into a set, removing any duplicates
;('A' || 'B')
;
;Parameters:
;    A (list): Set 1 to be combined
;    B (list): Set 2 to be combined
;
;Returns:
;    A new set containing all and only the elements of Of the union of 'A' and 'B'
;    That is, any element in either set

(define (s-union A B)
  
  ;Appends lists together then removes all duplicates
  (list-to-set (append A B))) 


;======
;s-diff
;======
;
;Desc: This function removes all members of 'A' that are present in set 'B'
;
;Parameters:
;    A (list): List of elements to remove
;    B (list):Set to remove elements from
;
;Returns:
;    A set containing all and only the elements of A - B

(define (s-diff A B)

  ;Base case of subtracting an empty entry
  (if (empty? B)

      ;A must be finished
      A          

      ;Otherwise remove the first entry of 'B' from 'A'
      ;then pass this and the rest
      (s-diff (s-remove (car B) A) (s-remove (car B) B)))) 


;========
;s-remove
;========
;
;Desc: This program removes 'a' from 'A' if it is present
;
;Parameters:
;    a (element): Element to remove nested in a list
;    A (list): Set to remove from
;
;Returns:
;    set A without a in it.

(define (s-remove a A)
  ;Define a quick not equal to a predicate
  (define (nota? x)     
    (not (eq? a x)))
  
  ;If its empty it just returns the original list
  (if (empty? a) A      
      (filter nota? A)))


;===========
;list-to-set
;===========
;
;Desc: Turns lists to sets by removing any duplicate entries
;
;Parameters: 
;    L (list) : A list with possible reoccuring entries to be transformed into a set
;
;Returns:
;    A new set based on the list L without any repeating elements.

(define (list-to-set L)
  
  ;First check if the list is empty
  (if (empty? L) '()
      
      ;Otherwise we recursively cons the first element of L
      ;with the filtered set of everything not equal to it.
      (cons (car L) (list-to-set 
                   (filter (lambda (x)
                             (not (equal? (car L) x))) L)))))



;==============
;get functions:
;======================================================================================



;=====================
;1. (get-variables G):
;=====================
;
; parameters:
;    G (list) : grammar set
;
; output:
;    Variables (list) : variables in our grammar set
;
; desc:
;    take a grammar as input and returns the set of variables
;    (i.e. the first element in the grammar 4-tuple).

(define (get-variables G)
  
  ;Raise error for incorrect inputs (Anything not a list)
  (if (not (list? G))
      (raise-argument-error get-variables "list?" G)
      
      ;Else return variables with list-ref
      (list-ref G 0)))



;====================
;2. (get-alphabet G):
;====================
;
; parameters:
;    G (list) : grammar set
;
; output:
;    Alphabet (list) : alphabet in our grammar set
;
; desc:
;    take a grammar as input and returns the set of terminals
;    (i.e. the second element in the grammar 4-tuple).

(define (get-alphabet G)
  
  ;Raise error for incorrect inputs
  (if (not (list? G))
      (raise-argument-error get-alphabet "list?" G)
      
      ;Reference sublist to return alphabet
      (list-ref G 1)))



;=================
;3. (get-rules G):
;=================
;
; parameters:
;    G (list) : grammar set
;
; output:
;    rules (list) : a list of rules in our grammar set
;
; desc:
;    take a grammar as input and returns the set of rules
;    (i.e. the third element in the grammar 4-tuple).

(define (get-rules G)
  
  ;Raise error for incorrect inputs
  (if (not (list? G))
      (raise-argument-error get-rules "list?" G)
      
      ;Reference sublist to return rules
      (list-ref G 2)))



;========================
;4. (get-start-symbol G):
;========================
;
; parameters:
;    G (list) : grammar set
;
; output:
;    start symbol (variable) : which variable is concidered start
;
; desc:
;    take a grammar as input and returns the start symbol
;    (i.e. the fourth element of the grammar 4-tuple).

(define (get-start-symbol G)
  
  ;Raise error for incorrect inputs
  (if (not (list? G))
      (raise-argument-error get-start-symbol "list?" G)
      
      ;Reference sublist to return start symbol.
      ;Note that this is the only function that does not return a list
      (list-ref G 3)))



;===========
;predicates:
;======================================================================================



;==========================
;5. (is-formal-grammar? G):
;==========================
;
; parameters:
;    G (list) : grammar set
;
; output:
;    Boolean : Whether or not list G is valid as grammar
;
; desc:
;    takes a grammar as input and returns true if it is a valid formal grammar
;    (and returns false otherwise). We say the Racket representation G of the
;    grammar is valid if all of the following are true
;
;        • G is a list of length four
;        • All but the last element of G is itself a list
;        • The last element of G is not a list
;        • Each of the rules in G is a list of lists
;        • Σ ∩ V = ∅
;        • S ∈ V



(define(is-formal-grammar? G)
  
  ;Raise error for incorrect inputs
  (if (not (list? G))
      (raise-argument-error is-formal-grammar? "list?" G)

      ;Check Formal Grammar requirements

      ;G is a list of length 4
      (and (eq? (length G) 4)

      ;all elements but the last are a list
      (and(list? (get-variables G))          
           (and(list? (get-alphabet G))    
               (and (list? (get-rules G))
                    

      ;last element is not a list
      (and (not (list? (get-start-symbol G)))

      ;Each of the rules in G is a list
      (and (andmap (lambda (x)
                     (list? x)) (get-rules G))
           
      ;Σ ∩ V = ∅
      (and (equal? (s-intersect (get-alphabet G)
                                (get-variables G))
                   '())

      ;S ∈ V
      (and (s-in? (get-start-symbol G)
                  (get-variables G))))))))))))   



;========================
;6. (is-context-free? G):
;========================
;
;Context-free Grammars:
;
;   Every context-free language can be described by a context-free grammar (CFG).
;   In a context-free grammar, every rule has the form v → t ,
;   wherev v ∈ V  and t ∈ ( Σ ∪ V ) ∗ . 
;   "Context free" refers to the fact that the production rules have a single
;   non-terminal on the left side, and thus can be rewritten regardless of
;   the "context" in which v exists.
;
;   The set of context-free languages is strictly a
;   superset of the set of regular languages.
;
;Program information:
;
; parameters:
;    G (list) : grammar set
;
; output:
;    Boolean : Whether or not list G is context free grammar
;
; desc:
;    takes a grammar as input and returns true if it is a formal grammar
;    and each rule fits the definition of context free grammar in the
;    Formal Grammar Guide; returns false otherwise.

(define (is-context-free? G)

  ;Raise error for incorrect inputs
  (if (not (list? G))
      (raise-argument-error is-context-free? "list?" G)

  ;Has to be real grammar to be context free
  (and(is-formal-grammar? G)

      ;Make sure its a list
      (and (andmap (lambda (x)
                      (list? x)) (get-rules G))

      ;And make sure all rules are of form v → t
      ;Means that the first thing must be a single non terminal
      (and(andmap (lambda (y)
             (equal? (length (car y))
                     1)) (get-rules G))

      ;v ∈ V
      (and (andmap (lambda (z)
                 ;check for v ∈ V since true of all forms
                 (s-in? z (get-variables G)))
               
            ;x mapping to list of all rules
            (apply append (map (lambda (u)
                       (car u))(get-rules G))))

      ;t ∈ ( Σ ∪ V ) ∗
      ;t is an element of the kleene star of the union of variables
      ;and alphabet. That is to say t is composed of elements from
      ;Σ and V of any length, and including the empty set.
      (andmap (lambda (w)
             (s-in? w (s-union (get-variables G)
                               (get-alphabet G))))

           ;x mapping
           ;Flattening here accounts for the empty string
           ;by removing them since they are allowed due
           ;to kleene star operation.
           (flatten (apply append (map (lambda (t)
                       (cdr t))(get-rules G)))))

))))))

;=================================
;7. (is-right-regular-grammar? G):
;=================================
;
; parameters:
;    G (list) : grammar set
;
; output:
;    Boolean : Whether or not list G is context free grammar
;
; desc:
;    takes a grammar as input and returns true if it is a context
;    free grammar and each rule fits the definition of right regular
;    grammar in the Formal Grammar Guide; returns false otherwise.

;A right regular grammar is a formal grammar
;where each of the rules has the one of the following forms:
;
;    v → a , where v ∈ V and a ∈ Σ , or
;    v → a u , where v, u ∈ V and a ∈ Σ , or
;    v → ε , where v ∈ V .

;That is to say, this grammar will only grow strings from left to right,
;with each rule being a terminal, terminal then symbol, or empty string
;to end.

(define (is-right-regular? G)

  ;Raise error for incorrect inputs
  (if (not (list? G))
      (raise-argument-error is-right-regular? "list?" G)

  ;check for context free
  (and (is-context-free? G)
       
       ;map x onto each rule in rule list of G
       (and (andmap (lambda (x)
                 ;check for v ∈ V since true of all forms
                 (s-in? x (get-variables G)))
               
            ;x mapping to list of all rules
            (apply append (map (lambda (u)
                       (car u))(get-rules G))))

            ;check right side of rule expressions
            ;Here we map an expression onto each of
            ;the rule outputs and check that they match
            ;at least one of the possible forms.
            (andmap (lambda (y)
                      (or (is-form-1? y G)
                          (or (is-form-2? y G)
                              (or (is-form-3? y G)))))

             ;y mapped to this list
             (apply append (map (lambda (w)
                       (cdr w))(get-rules G))))))))


;Helper functions to is-right-regular?
;for looking at the right side of rules

;=============
;Form 1: a ∈ Σ
;=============
;
; parameters:
;    G (list) : grammar set
;    Rule (list): the rule being evaluated
;
; output:
;    BOOLEAN: Whether or not the rule is of the form a ∈ Σ
;
; desc:
;    This function is only ever handed a single right side
;    of a rule, it then checks that it is only one item long
;    and in the set of the alphabet of G


(define (is-form-1? rule G)
  
  ;rule sublists is length 1
  (and (eq? (length rule) 1)
       
       ;rule is a terminal
       (s-in? (list-ref rule 0) (get-alphabet G))))


;=======================
;Form 2: u ∈ V and a ∈ Σ
;=======================
;
; parameters:
;    G (list) : grammar set
;    Rule (list): the rule being evaluated
;
; output:
;    BOOLEAN: Whether or not the rule is of the form (a,u),
;    where u ∈ V and a ∈ Σ.
;
; desc:
;    This function is only ever handed a single right side
;    of a rule, it then checks that it is two items long
;    and if the first member is a variables and the second
;    member an alphabet of G.

(define (is-form-2? rule G)
  
  ;rule sublists is length 2
  (and (eq? (length rule) 2)
       
       ;car of rule is a terminal
       (and (s-in? (car rule) (get-alphabet G))
            
            ;and cdr of rule is another variable
            (s-in? (list-ref (cdr rule) 0) (get-variables G)))))


;=========
;Form 3: ε
;=========
;
; parameters:
;    G (list) : grammar set
;    Rule (list): the rule being evaluated
;
; output:
;    BOOLEAN: Whether or not the rule is of the form v → ε
;
; desc:
;    This function is only ever handed a single right side
;    of a rule, it then checks that it is of length 0. Only
;    the empty set is of length 0.

(define (is-form-3? rule G)
  
  ;rule sublist is length 0
  (eq? (length rule) 0))



;===========================
;(generate-random-string G):
;======================================================================================
; parameters:
;    G (list) : grammar set
;
; output:
;    String: A random string generated from the grammar set
;
; desc:
;    takes a context-free grammar G and produces a string in the language,
;    where the string is represented as a list of symbols in the alphabet.
;    It should start with the start symbol, and always rewrite the
;    left-most non-terminal, randomly picking among the rewrite rules for
;    that non-terminal see (random n)). It only supports context-free
;    grammars; if passed a grammar that is not context-free,
;    it should return an empty list.
;
;    My implimentation calls a nested recursive function to facilitate
;    passing the string between recursive itterations. This function is
;    also assisted by two helper functions, one to swap the variables
;    according to their rules, and another small predicate to give to
;    filter when searching through the rules lists.

(define (generate-random-string G)

  ;Nested Function: string-generator
  ;
  ; paramters:
  ;
  ;    string (LIST): The string which will be recursively built
  ;                   and returned. Upon calling it must be the start
  ;                   symbol put into a list. This is so it can 
  ;                   recursively append to the input.
  ;
  ;    G (LIST):      The grammar whos rules we shall use to construct
  ;                   an acceptable and random string.
  ;
  ; outputs:
  ;
  ;    string (LIST): The string built by starting with the start symbol
  ;                   and applying random rule sets to until complete.
  ;             
  ;recursively call grammars with appended string until finished
  (define (string-generator string G)

    ;First check context free
    (if (is-context-free? G)
          
        ;Then check for base case
        (if (ormap (lambda (x)
                     (s-in? x (get-variables G)))

                   ;x is mapped onto string 
                   string)

            ;Then do recursive call last for tail recursion
            (string-generator

             ;Pass in altered string
             (append-map (lambda (z)

                           ;If in variables *see nested function
                           (if (in-variables? z G)

                               ;Then replace
                               (replace-var z G)

                               ;Else just return character
                               (list z)))
                         string)
             G)

              
            ;else
            string)
 
        ;else return an empty list
        '()))


  ;Nested Function: replace-var
  ;
  ; paramters:
  ;
  ;    x (SYMBOL):    A symbol to be replaced with one of its rules 
  ;
  ;    G (LIST):      The grammar whos rules we shall use.
  ;
  ; outputs:
  ;
  ;    rule (LIST):   A random one of x's rules from G.
  ;                   
  ;Here x is an individual symbol from our list
  (define (replace-var x G)

    ;define a nested predicate defined upon execution
    (define (eqX? a)
      (equal? a x))
  
    ;Select a rule from corresponding rule list at random
    (list-ref
   
     ;Corresponding rule list
     (cdr (list-ref (get-rules G)
                    (index-of
                     (apply append
                            (map (lambda (y)
                                   (car y))
                                 (get-rules G)))
                     x)))

     ;Randomized list-ref
     ;Random num from 0 to length of list - 1
     (random 0 (- (length (list-ref (get-rules G)
                                    (index-of
                                     (apply append
                                            (map (lambda (y)
                                                   (car y))
                                                 (get-rules G)))
                                     x))) 1))))

  ;Nested Function: in-variables?
  ;
  ; paramters:
  ;
  ;    x (SYMBOL):    A symbol from our string list 
  ;
  ;    G (LIST):      The grammar whos variables we shall use.
  ;
  ; outputs:
  ;
  ;    BOOLEAN:       Whether or not x is a variable under grammar G.
  ;                   
  ;Here x is an individual symbol from our list
  (define (in-variables? x G)
    (s-in? x (get-variables G)))


  ;Here is the initial call to string-generator
  ;from generate-random-string.
  
  ;Raise error for incorrect inputs
  (if (not (list? G))
      (raise-argument-error get-variables "list?" G)

  ;Call recursive function defined above
  (string-generator (list (get-start-symbol G)) G)))