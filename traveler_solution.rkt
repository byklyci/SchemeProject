#lang scheme
;2016400357


;CMPE260 SCHEME PROJECT IMPLEMENTATION MADE BY YUSUF KALAYCI WITH SLEEPLES NIGHTS....

;Railway connection
;The railway connection is find with the use of car,cdr and their extended forms of like caddr,cadddr etc. to reach the desired part of database.
;There is a inner function to reach the database.
;The main thought behind my implementation is recursion among the whole function begin to end.
;If statement used to check the correct position with the help of car,cdr and if the database(city) is not found there will be null. 
(define (RAILWAY-CONNECTION database)
      (RAILWAY-CONNECTION-city LOCATIONS database))


(define (RAILWAY-CONNECTION-city list database)
    (if (null? list)
        null
        (if(eqv? database (car(car list)))
                  (caddr(car list))                   	          
                  (RAILWAY-CONNECTION-city (cdr list) database))))

;Accommodation cost
;The accomodation cost is find the cost of accomodation place with use of car,cdr and their extended forms like caddr,caddr etc.to reach the desired part of database.
;There is an inner function to reach the database just above.
;The main thought behind my implementation is recursion among the whole function begin to end.
;If statement used to check the correct position with the help of car,cdr and if it can be found in the database(city).
(define (ACCOMMODATION-COST database)
  (ACCOMMODATION-COST-city LOCATIONS database))

(define (ACCOMMODATION-COST-city list database)
    (if(eqv? database (car (car list)))
                    (cadr(car list))                    	          
                  (ACCOMMODATION-COST-city (cdr list) database)))

;Interested cities
;The interested city is find the cities of person in the database.I used car,cdr and their extended forms like caddr,caddr etc. to reach the desired part of database.
;There is an inner function to reach the database just above.
;The main thought behind my implementation is recursion among the whole function begin to end.
;If statement used to check the correct position with the help of car,cdr and if the database(person's interest) is not found there will be null.
(define (INTERESTED-CITIES database)
  (INTERESTED-CITIES-person TRAVELERS database))

(define (INTERESTED-CITIES-person list database)
  (if (null? list)
      null
    (if(eqv? database (car (car list)))
                     (cadr(car list))                    	          
                  (INTERESTED-CITIES-person (cdr list) database))))

;Interested activities
;The interested activity is find the cities of person interest in the database.I used car,cdr and their extended forms like caddr,caddr etc. to reach the desired part of database.
;There is an inner function to reach the database just above.
;The main thought behind my implementation is recursion among the whole function begin to end.
;If statement used to check the correct position with the help of car,cdr and if the database(person's interested activity) is not found there will be null.
(define (INTERESTED-ACTIVITIES database)
  (INTERESTED-ACTIVITIES-person TRAVELERS database))
(define (INTERESTED-ACTIVITIES-person list database)
  (if (null? list)
      null
    (if(eqv? database (car (car list)))
                    (caddr(car list))                    	          
                  (INTERESTED-ACTIVITIES-person (cdr list) database))))

;Home
;The home is find the home place of traveler with use of car,cdr and their extended forms like caddr,caddr etc.to reach the desired part of database.
;There is an inner function to reach the database just above.
;The main thought behind my implementation is recursion among the whole function begin to end.
;If statement used to check the correct position with the help of car,cdr and if it can be found database(home-traveler).
(define (HOME database)
  (HOME-person TRAVELERS database))
(define (HOME-person list database)
    (if(eqv? database (car (car list)))
                    (cadddr(car list))                    	          
                  (HOME-person (cdr list) database)))

;Traveller From
;The traveller from is find the traveler person travel place in the database.I used car,cdr and their extended forms like caddr,caddr etc. to reach the desired part of database.
;There is an inner function to reach the database just above.
;The main thought behind my implementation is recursion among the whole function begin to end.And the use of map,filter form with lambda.
;I filter the database list recursively and map the right block to achieve travelers whose hometown is that location with use of single location argument.
(define (TRAVELER-FROM database)
  (map (lambda (trav_name) (car trav_name))
       (TRAVELER-FROM-city database TRAVELERS)))
(define (TRAVELER-FROM-city database list)
    (filter (lambda (location) (eqv? database (car(cdddr location)))) list))

;Interested-in-city
;The interested in citytakes a single argument (a city) and returns a list of travelers
;who wants to visit that city.I used car,cdr and their extended forms like caddr,caddr etc. to reach the desired part of database.
;There is an inner function to reach the database just above.
;The main thought behind my implementation is recursion among the whole function begin to end.And the use of memq and map,filter form with lambda.
;I filter the database list recursively and map the right block to achieve list of travelers
;who wants to visit that city with use of single city argument.
(define (INTERESTED-IN-CITY database)
  (map (lambda (trav_name) (car trav_name))
       (INTERESTED-IN-CITY-person database TRAVELERS)))
(define (INTERESTED-IN-CITY-person database list)
    (filter (lambda (location) (memq database (cadr location))) list))
  
;Interested-in-activity 
;The interested in city takes a single argument (an activity) and returns a list of travelers who enjoy that activity.
;I used car,cdr and their extended forms like caddr,caddr etc. to reach the desired part of database.
;There is an inner function to reach the database just above.
;The main thought behind my implementation is recursion among the whole function begin to end.And the use of memq and map,filter form with lambda.
;I filter the database list recursively and map the right block to achieve list of travelers
;who wants to visit that city with use of single city argument.
(define (INTERESTED-IN-ACTIVITY database)
  (map (lambda (trav_name) (car trav_name))
       (INTERESTED-IN-ACTIVITY-person database TRAVELERS)))
(define (INTERESTED-IN-ACTIVITY-person database list)
    (filter (lambda (location) (memq database (caddr location))) list))

;Railway network
;The railway network is akes a single argument (a location) and returnsa list of locations which are accessible from this location by train. The list excludes the given location.
;I call realway connection function in the railway network connection then ı control with the help of another list for not only network-connection but also inside the list's whole connection
;I used the memq and cons to make my control if statements look at the lists and control if sth else sth for the reach whole network among the connection.
(define (RAILWAY-NETWORK my_city)
  (RAILWAY-NETWORK-founder (RAILWAY-CONNECTION my_city) my_city))
(define(RAILWAY-NETWORK-founder list my_city)
   (if (null? list)
        null
      (addition (addition list (RAILWAY-CONNECTION (car list)) my_city) (RAILWAY-NETWORK-founder (cdr list ) my_city) my_city)))

(define (addall list newlist my_city)
  (if (null? newlist)
      null (list)))
(define (addition list newlist my_city)
  (if (null? newlist )
      list  
      (if (or(memq (car newlist) list) (eqv? (car newlist) my_city))
          (addition list (cdr newlist) my_city)
            (addition (cons (car newlist) list) (cdr newlist) my_city))))

;Accomation expenses
;takes two arguments (a traveler and a location) and returns the accommodation cost for the traveler. If the location is his/her hometown, then
;no funding is necessary. If the location is another city, then the traveler stays in hotel where one night cost is given in the LOCATIONS database. If the location hosts an interesting
;activity for the traveler, then the traveler stays for three nights to enjoy more, otherwise he/she stays only for one night.First of all control the it is home or not with if statement
;then if it is member of another city it calls recursively other cities cost then multiple by 3 them for hotel stay.
(define (ACCOMMODATION-EXPENSES name cities)
  (ACCOMMODATION-EXPENSES-name name cities (INTERESTED-ACTIVITIES name)))
(define (ACCOMMODATION-EXPENSES-name name cities list)
  (if (equal? (HOME name) cities)
      '0
          (if (memq cities (INTERESTED-IN-CITY cities))
              (* 3 (ACCOMMODATION-COST cities))
              (ACCOMMODATION-COST  cities))))

;Traveler expenses
;This function takes two arguments (a traveler and a location) and returns the
;travel cost for the visitor. If the visited city resides in the railway network of the traveler’s
;hometown, then it costs only 50 units (“Aktarma” between trains is free of charge). Otherwise,
;the traveler uses airlines which cost 100 units. Recall that the traveler uses the same way to
;return back to home.Call the home function and make if control statement then write the value of these costs.
(define (TRAVEL-EXPENSES name_of_person cities)
  (if(eqv? (HOME name_of_person) cities)
     '0
     (if(memq cities (RAILWAY-NETWORK (HOME name_of_person)))
        '100
        '200)))


;Expenses
;takes two arguments (a traveler and a location) and returns the sum of travel cost and accommodation cost for the traveler.
;it looks also the same in below then try to sum accomodation expenses and travel expenses with
;the + operation if it is true.If statement for control mechanism if eqv home then return 0 else do the operation 

(define (EXPENSES name_of_person cities)
  (if(eqv? (HOME name_of_person) cities)
     '0
     (if(memq cities (RAILWAY-NETWORK (HOME name_of_person)))
        #t (if (memq cities (INTERESTED-IN-CITY cities))
              (+ (ACCOMMODATION-COST cities))
              (ACCOMMODATION-COST  cities)))))



;In between
;This function control the Accomodation cost between two value and make comparison
;if it is inside the between part show the cities between these accomodation. 
(define (IN-BETWEEN  limita limitb)
  (IN-BETWEEN-helper LOCATIONS limita limitb))
(define (IN-BETWEEN-helper list limita limitb)
  (if (null? list)
      null
      (if (eq? "between" (compare-number (ACCOMMODATION-COST (caar list)) limita limitb)) 
               (cons (caar list) (IN-BETWEEN-helper (cdr list) limita limitb))
               (IN-BETWEEN-helper (cdr list) limita limitb))))
           
(define (compare-number z x y)
  (if (or(and (> z x) (< z y)) (and (< z x) (> z y)))
      (print "between")
      null))

  






