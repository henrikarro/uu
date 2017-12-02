#lang s-exp "relations.rkt"

(relation pers (make-relation
                (name title department salary)
                (("John" "Accountant" "Finance" 3000)
                 ("Rob" "Salesman" "Sales" 5000)
                 ("Bill" "Manager" "Sales" 10000)
                 ("Ben" "Driver" "Logistics" 4500))))
(relation dept (make-relation
                (department location head)
                (("Finance" "Paris" "John")
                 ("Sales" "London" "Bill")
                 ("It-support" "Paris" "Mark"))))
(show (project (restrict pers (salary . >= . 5000)) (name title)))
(equal? (project dept (location))
        (make-relation (location)
                       (("Paris") ("London"))))

(relation frotz (make-relation
                 (foo bar baz)
                 ((1 2 3) (4 5 6))))

(restrict frotz (< foo 3))
(equal? (project frotz (baz foo))
        (make-relation (foo baz) ((4 6) (1 3))))
(equal? (project frotz (baz foo))
        (make-relation (foo baz) ((4 6) (1 2))))
