       IDENTIFICATION DIVISION.
       PROGRAM-ID. JJONESPROJECT3.
       AUTHOR. JONES.
      ********************************************************************
      *                     PROJECT 3 CS 370-001
      *	This program will produce a set of order reports for Dr.Cheeb's 
      * new herbal medicine product for the last 3 months. It will sort the
      * incoming input file "PR3FA17.TXT" by Warehouse State, City, and
      * Customer ID.
      ********************************************************************
      * INPUTS: UNSORT-ORDER-FILE
      * 	WAREHOUSE-STATE 	ALPHABETIC   1-2 (AL, GA)
      *		WAREHOUSE-CITY      ALPHANUMERIC 4-5 (BM,HU,MO,AT,SA,VA)
      *		CUSTOMER-ID         ALPHANUMERIC 7-8 
      *     CUSTOMER-NAME       ALPHANUMERIC 9-28 
      *     CUSTOMER-RATING     NUMERIC      29
      *		PRODUCT DATA                     31-126 (MAXIMUM OF 6 ARRAY)
      *     	PROD-ID		    X(5)
      *         PROD-CODE       X PROD-TYPE:(E-EDIBLES , O-OILS, C-CAPSULES) 
      *         NUM-OF-BOXES    9(3)
      *         PRICE-PER-BOX   999V99
      *         MONTH-BOUGHT    99 (VALID MONTHS ARE 1-12)
      *
      * INPUT: SORTED-ORDER-FILE 
      ********************************************************************
      * OUTPUT: ORDER-REPORT-RECORD
      * 	REPORT TITLE
      *     PAGE NUMBER
      *     WAREHOUSE STATE
      *     WAREHOUSE CITY
      *     PRODUCT ID
      *     CUTOMER NAME
      *     CUSTOMER RATING
      *     PRODUCT TYPE
      *     MONTH BOUGHT
      *	    NUM SOLD
      *     BOX PRICE
      *     TOTAL SOLD PER MONTH
      *     TOTAL VALUE
      ***************************************************************
      * CALCULATIONS:
      *		TOTAL SOLD VALUE = NUM-OF-BOXES * BOX-PRICE
      ***************************************************************
       ENVIRONMENT DIVISION.
	   
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	         SELECT UNSORT-ORDER-FILE
                ASSIGN TO 'PR3FA17.txt'
                ORGANIZATION IS LINE SEQUENTIAL.
				
             SELECT SORT-FILE
                ASSIGN TO  'SORT.TMP'.
				
				
             SELECT SORTED-ORDER-FILE
                ASSIGN TO 'S-PR3FA17.txt'
                ORGANIZATION IS LINE SEQUENTIAL.
				
             SELECT ORDER-REPORT-RECORD
                ASSIGN TO PRINTER 'ORDER-REPORT.txt'
                ORGANIZATION IS LINE SEQUENTIAL.
				
       DATA DIVISION.
       FILE SECTION.
            FD UNSORT-ORDER-FILE
                RECORD CONTAINS 126 CHARACTERS.
				
            01 UNSORT-ORDER-RECORD.
                05 UOR-STATE	    PIC A(2).
                05 FILLER           PIC X.
				05 UOR-CITY         PIC X(2).
				05 FILLER           PIC X.
				05 UOR-CUST-ID      PIC X(2).
				05 UOR-CUST-NAME    PIC X(20).
				05 UOR-CUST-RATING  PIC X.
				05 FILLER           PIC X.
				05 UOR-ORDER-ARRAY  OCCURS 6 TIMES.
					10 UOR-PROD-ID      PIC X(5).
					10 UOR-PROD-CODE    PIC X.
					10 UOR-NUM-BOXES    PIC 9(3).
					10 UOR-PRICE-PER    PIC 999V99.
					10 UOR-MONTH-BOUGHT PIC 99.
				
			SD SORT-FILE
				RECORD CONTAINS 126 CHARACTERS.
				
			01 SORT-RECORD.
				05 SR-STATE        PIC A(2).
				05 FILLER          PIC X.
				05 SR-CITY         PIC X(2).
				05 FILLER          PIC X.
				05 SR-CUST-ID      PIC X(2).
				05 SR-CUST-NAME    PIC X(20).
				05 SR-CUST-RATING  PIC X.
				05 FILLER          PIC X.
				05 SR-ORDER-ARRAY  OCCURS 6 TIMES.
					10 SR-PROD-ID      PIC X(5).
					10 SR-PROD-CODE    PIC X.
					10 SR-NUM-BOXES    PIC 9(3).
					10 SR-PRICE-PER    PIC 999V99.
					10 SR-MONTH-BOUGHT PIC 99.
					
			FD SORTED-ORDER-FILE
				RECORD CONTAINS 126 CHARACTERS.
				
			01 SORTED-ORDER-RECORD.
				05 SOR-STATE        PIC A(2).
				05 FILLER           PIC X.
				05 SOR-CITY         PIC X(2).
				05 FILLER           PIC X.
				05 SOR-CUST-ID      PIC X(2).
				05 SOR-CUST-NAME    PIC X(20).
				05 SOR-CUST-RATING  PIC X.
				05 FILLER           PIC X.
				05 SOR-ORDER-ARRAY  OCCURS 6 TIMES.
					10 SOR-PROD-ID      PIC X(5).
					10 SOR-PROD-CODE    PIC X.
					10 SOR-NUM-BOXES    PIC 9(3).
					10 SOR-PRICE-PER    PIC 999V99.
					10 SOR-MONTH-BOUGHT PIC 99.
				
            FD ORDER-REPORT-RECORD
                RECORD CONTAINS 126 CHARACTERS.
				
            01 REPORT-RECORD        PIC X(126).
			
       WORKING-STORAGE SECTION.
            01 FLAGS-N-SWITCHES.
                05 EOF-FLAG         PIC X		VALUE 'N'.
                05 FIRST-RECORD     PIC X		VALUE 'Y'.
                05 GROUP-FLAG       PIC X       VALUE 'N'.
				
            01 SUB                  PIC 99      VALUE 0.

            01 CUST-RATING-NUMBER.
                05                  PIC X(11)   
                      VALUE '1Aggressive'.
                05                  PIC X(11)       
                      VALUE '2Active'.
                05                  PIC X(11)       
                      VALUE '3Moderate'.
                05                  PIC X(11)      
                      VALUE '4Inactive'.

				
            01 CUST-RATING-TABLE REDEFINES CUST-RATING-NUMBER.
                05 RATING-ITEM OCCURS 4 TIMES INDEXED BY RATING-INDEX.
                    10 CRT-NUMBER   PIC X.
					10 CRT-NAME     PIC X(10).
					
            01 REPORT-FIELDS.
                05 PROPER-SPACING   PIC S9      VALUE +1.
                05 LINE-COUNT       PIC S9(2)   VALUE +0.
                05 PAGE-NO          PIC S9(2)   VALUE +0.
                05 RF-PROD-TYPE     PIC X(8)    VALUE ' '.
                05 RF-MONTH-BOUGHT  PIC X(9)    VALUE ' '.
				
            01 WS-HOLD.
                05 CUST-HOLD         PIC X(2)    VALUE ' '.
                05 WSHTEMPCITY       PIC X(2)    VALUE ' '.
                05 WSHTEMPSTATE      PIC A(2)    VALUE ' '.
                05 WSHTEMPMONTOTAL   PIC 9(8)V99 VALUE ZEROES.
				
            01 DETAIL-FIELDS.
                05 DF-BOXES-SOLD       PIC 9(3)    VALUE ZEROES.
                05 DF-PRICE-PER-BOX    PIC 9(3)V99 VALUE ZEROES.
				
            01 TOTAL-FIELDS.
                05 TF-MONTH-VAL        PIC 9(6)V99   VALUE ZEROES.
                05 TF-TOTAL-SOLD-VAL   PIC 9(7)V99   VALUE ZEROES.
                05 TF-CITY-VAL         PIC 9(8)V99   VALUE ZEROES.
                05 TF-STATE-VAL        PIC 9(11)V99  VALUE ZEROES.
				
            01 GRAND-TOTAL-FIELDS.
                05 GT-TOTAL            PIC 9(14)V99  VALUE ZEROES.
				
            01 HEADER-LINE1.
                05 FILLER              PIC X(26).
                05                     PIC X(10)     VALUE 'DR. CHEEBS'.
				
            01 HEADER-LINE2.
                05 FILLER              PIC X(20).
                05                     PIC X(20)
            	       VALUE 'NEW PRODUCT ANALYSIS'.
                05 FILLER              PIC X(9).
                05                     PIC X(5)      VALUE 'PAGE:'.
                05 HL2-PAGE-NUM        PIC X(2).
				
            01 STATE-NAME-LINE.
                05                     PIC X(6)      VALUE 'STATE:'.
                05 FILLER              PIC X.
                05 SNL-STATE           PIC X(7).
				
            01 CITY-NAME-LINE.
                05 FILLER              PIC X.
                05                     PIC X(5)      VALUE 'CITY:'.
                05 FILLER              PIC X.
                05 CNLCITY             PIC X(10).
				
            01 ID-LINE.
                05 FILLER              PIC X(5).
                05                     PIC X(3)      VALUE 'ID:'.
                05 FILLER              PIC X(2).
                05 IDL-PROD-ID         PIC X(5).
				
            01 NAME-LINE.
                05 FILLER              PIC X(3).
                05                     PIC X(5)      VALUE 'NAME:'.
                05 FILLER              PIC X(2).
                05 NL-NAME             PIC X(19).
				
            01 RATING-LINE.
                05 FILLER              PIC X.
                05                     PIC X(7)      VALUE 'RATING:'.
                05 FILLER              PIC X(2).
                05 RL-RATING           PIC X(10).
				
            01 HEADER-LINE3.
                05 FILLER              PIC X.
                05                     PIC X(15)     
        		        VALUE 'P R O D U C T S'.
                05 FILLER              PIC X(3).
                05                     PIC X(5)      VALUE 'MONTH'.
                05 FILLER              PIC X(5).
                05                     PIC X(3)      VALUE 'NUM'.
                05 FILLER              PIC X(5).
                05                     PIC X(3)      VALUE 'BOX'.
                05 FILLER              PIC X(5).
                05                     PIC X(10)
                        VALUE 'TOTAL SOLD'.
						
            01 HEADER-LINE4.
                05 FILLER              PIC X(3).
                05                     PIC X(2)      VALUE 'ID'.
                05 FILLER              PIC X(5).
                05                     PIC X(4)      VALUE 'TYPE'.
                05 FILLER              PIC X(5).
                05                     PIC X(6)      VALUE 'BOUGHT'.
                05 FILLER              PIC X(4).
                05                     PIC X(4)      VALUE 'SOLD'.
                05 FILLER              PIC X(3).
                05                     PIC X(5)      VALUE 'PRICE'.
                05 FILLER              PIC X(4).
                05                     PIC X(9)      
                        VALUE 'PER MONTH'.
						
            01 DETAIL-LINE.
                05 FILLER              PIC X.
                05 DL-PROD-ID          PIC X(5).
                05 FILLER              PIC X(2).
                05 DL-PROD-TYPE        PIC X(8).
                05 FILLER              PIC X(2).
                05 DL-MONTH-BOUGHT     PIC X(9).
                05 FILLER              PIC X(3).
                05 DL-NUM-SOLD         PIC ZZ9.
                05 FILLER              PIC X(3).
                05 DL-BOX-PRICE        PIC ZZ9.99.
                05 FILLER              PIC X(3).
                05 DL-TOT-SOLD-PER     PIC $ZZZ,ZZ9.99.
				
            01 TOTAL-LINE.
                05 FILLER              PIC X(27).
                05                     PIC X(6)      VALUE 'TOTAL:'.
                05 FILLER              PIC X(10).
                05 TL-TOTAL-SOLD       PIC $$,$$$,$$9.99.
				
            01 CITY-TOTAL-LINE.
                05 FILLER              PIC X(8).
                05                     PIC X(7)
                      VALUE 'CITY OF'.
                05 FILLER              PIC X.
                05 CTL-CITY-NAME       PIC X(10).
                05 FILLER              PIC X.
                05                     PIC X(6)      VALUE 'TOTAL:'.
                05 FILLER              PIC X(9).
                05 CTL-CITY-TOTAL      PIC $$$,$$$,$$9.99.
				
            01 STATE-TOTAL-LINE.
                05 FILLER              PIC X(10).
                05                     PIC X(8)
                      VALUE 'STATE OF'.
                05 FILLER              PIC X.
                05 STL-STATE-NAME      PIC X(7).
                05 FILLER              PIC X.
                05                     PIC X(6)      VALUE 'TOTAL:'.
                05 FILLER              PIC X(5).
                05 STL-STATE-TOTAL     PIC $$$,$$$,$$$,$$9.99.
				
            01 GRAND-TOTAL-LINE.
                05 FILLER              PIC X(21).
                05                     PIC X(12)
                     VALUE 'GRAND TOTAL:'.
                05 FILLER              PIC X.
                05 GTL-TOTAL           PIC $$$,$$$,$$$,$$$,$$9.99.
				
			
       PROCEDURE DIVISION.
	   
            100-MAIN-ROUTINE.
                PERFORM 125-SORT-FILE
				PERFORM 150-HOUSEKEEPING
                PERFORM 200-READ-ORDER-FILE
				PERFORM 900-EOF-ROUTINE
				STOP RUN
            .
			
            125-SORT-FILE.
                SORT SORT-FILE
                ON ASCENDING KEY  SR-STATE
                                  SR-CITY
                                  SR-CUST-ID
                USING  UNSORT-ORDER-FILE
                GIVING SORTED-ORDER-FILE
				
			.
			
            150-HOUSEKEEPING.
			    OPEN INPUT SORTED-ORDER-FILE
				     OUTPUT ORDER-REPORT-RECORD
				
                PERFORM 175-HEADING-ROUTINE
				
			.
			
			175-HEADING-ROUTINE.
			    MOVE 0 TO LINE-COUNT
				ADD 1 TO PAGE-NO
				MOVE PAGE-NO TO HL2-PAGE-NUM
				MOVE 1 TO PROPER-SPACING
				WRITE REPORT-RECORD FROM HEADER-LINE1
                     AFTER ADVANCING PROPER-SPACING
                WRITE REPORT-RECORD FROM HEADER-LINE2
				ADD 3 TO LINE-COUNT
				
			.
			
			200-READ-ORDER-FILE.
			    PERFORM UNTIL EOF-FLAG = 'Y'
				     READ SORTED-ORDER-FILE
                          AT END
                               MOVE 'Y' TO EOF-FLAG
                          NOT AT END
						       PERFORM 300-PROC
                     END-READ
                END-PERFORM
				
                PERFORM 650-STATEBREAK
				PERFORM 700-CALCGRANDTOTAL
				
			.
			
            300-PROC.
                IF LINE-COUNT >= 49
                     PERFORM 175-HEADING-ROUTINE
                END-IF
				
                EVALUATE TRUE
                     WHEN FIRST-RECORD = 'Y'
					      MOVE 'N' TO FIRST-RECORD
                          MOVE SOR-CUST-ID TO CUST-HOLD
                          MOVE SOR-CITY TO WSHTEMPCITY
                          MOVE SOR-STATE TO WSHTEMPSTATE
                          PERFORM 325-STATE-VALIDATE
					      PERFORM 350-CITY-VALIDATE
                          PERFORM 375-CUST
                          PERFORM 425-HEADER
                     WHEN SOR-STATE NOT = WSHTEMPSTATE
                          PERFORM 650-STATEBREAK
						  PERFORM 325-STATE-VALIDATE
						  PERFORM 350-CITY-VALIDATE
						  PERFORM 375-CUST
						  PERFORM 425-HEADER
                     WHEN SOR-CITY NOT = WSHTEMPCITY
                          PERFORM 625-CITYBREAK
                          PERFORM 350-CITY-VALIDATE
                          PERFORM 375-CUST
                          PERFORM 425-HEADER
                     WHEN SOR-CUST-ID NOT = CUST-HOLD
                          PERFORM 600-IDBREAK
                          PERFORM 375-CUST
                          PERFORM 425-HEADER
                 END-EVALUATE
				PERFORM 450-ARRAY
			.
			
			325-STATE-VALIDATE.
                 IF SOR-STATE = 'AL'
                      MOVE 'ALABAMA' TO SNL-STATE
					  MOVE 'ALABAMA' TO STL-STATE-NAME
                 ELSE
                      IF SOR-STATE = 'GA'
                           MOVE 'GEORGIA' TO SNL-STATE
						   MOVE 'GEORGIA' TO STL-STATE-NAME
                      ELSE
                           MOVE 'BAD' TO SNL-STATE
                      END-IF
                 END-IF
				 MOVE 1 TO PROPER-SPACING
                 WRITE REPORT-RECORD FROM STATE-NAME-LINE
                      AFTER ADVANCING PROPER-SPACING
			     ADD PROPER-SPACING TO LINE-COUNT
				 

            .				
			
			350-CITY-VALIDATE.
			    EVALUATE TRUE
                     WHEN SOR-CITY = 'BM'
				          MOVE 'BIRMINGHAM' TO CNLCITY
                          MOVE 'BIRMINGHAM' TO CTL-CITY-NAME
				     WHEN SOR-CITY = 'HU'
				          MOVE 'HUNTSVILLE' TO CNLCITY
						  MOVE 'HUNTSVILLE' TO CTL-CITY-NAME
				     WHEN SOR-CITY = 'MO'
					      MOVE 'MOBILE' TO CNLCITY
						  MOVE 'MOBILE' TO CTL-CITY-NAME
				     WHEN SOR-CITY = 'AT'
					      MOVE 'ATLANTA' TO CNLCITY
						  MOVE 'ATLANTA' TO CTL-CITY-NAME
					 WHEN SOR-CITY = 'SA'
					      MOVE 'SAVANNAH' TO CNLCITY
						  MOVE 'SAVANNAH' TO CTL-CITY-NAME
					 WHEN SOR-CITY = 'VA'
					      MOVE 'VALDOSTA' TO CNLCITY
						  MOVE 'VALDOSTA' TO CTL-CITY-NAME
					 WHEN OTHER
				          MOVE 'BAD' TO CNLCITY
				END-EVALUATE
				MOVE 2 TO PROPER-SPACING
				WRITE REPORT-RECORD FROM CITY-NAME-LINE
				     AFTER ADVANCING PROPER-SPACING
			    ADD PROPER-SPACING TO LINE-COUNT
				MOVE 1 TO PROPER-SPACING
				
            .

            375-CUST.
                 MOVE SOR-CUST-ID TO IDL-PROD-ID
                 MOVE SOR-CUST-NAME TO NL-NAME
				 PERFORM 400-TABLE-SEARCH
				 MOVE 1 TO PROPER-SPACING
				 WRITE REPORT-RECORD FROM ID-LINE
				      AFTER ADVANCING PROPER-SPACING
				 ADD 2 TO LINE-COUNT
                 MOVE 1 TO PROPER-SPACING				 
                 WRITE REPORT-RECORD FROM NAME-LINE
				      AFTER ADVANCING PROPER-SPACING
				 WRITE REPORT-RECORD FROM RATING-LINE
				      AFTER ADVANCING PROPER-SPACING
				 ADD 2 TO LINE-COUNT
				 
			.
                 				 
            

            400-TABLE-SEARCH.
                 SET RATING-INDEX TO 1
                 SEARCH RATING-ITEM
                 AT END
                      MOVE 'BAD' TO RL-RATING
                 WHEN SOR-CUST-RATING = CRT-NUMBER(RATING-INDEX)
                      MOVE CRT-NAME(RATING-INDEX) TO RL-RATING 
                 END-SEARCH
             
            .
			
			425-HEADER.
			     MOVE 1 TO PROPER-SPACING
			     WRITE REPORT-RECORD FROM HEADER-LINE3
				      AFTER ADVANCING PROPER-SPACING
                 WRITE REPORT-RECORD FROM HEADER-LINE4
                      AFTER ADVANCING PROPER-SPACING
                 ADD 4 TO LINE-COUNT
				 
			.
			
			450-ARRAY.
                 PERFORM VARYING SUB FROM 1 BY 1
                      UNTIL SUB > 6
					  IF SOR-MONTH-BOUGHT(SUB) > 07 AND < 11
                           PERFORM 475-PRODIDVALIDATE
                           PERFORM 500-NUMBOXVALIDATE
					       PERFORM 525-PRICEVALIDATE
                           PERFORM 550-MONTHVALIDATE
						   PERFORM 575-CALCMONTH
                           MOVE 1 TO PROPER-SPACING
                           WRITE REPORT-RECORD FROM DETAIL-LINE
                                AFTER ADVANCING PROPER-SPACING
                           ADD PROPER-SPACING TO LINE-COUNT
                           MOVE ZEROES TO DL-TOT-SOLD-PER
                           MOVE ZEROES TO TF-MONTH-VAL
                         
                      END-IF
                 END-PERFORM 
					  
			.
			
			475-PRODIDVALIDATE.
                 MOVE SOR-PROD-ID(SUB) TO DL-PROD-ID
                      EVALUATE TRUE
                           WHEN SOR-PROD-CODE(SUB) = "E"
                              MOVE "EDIBLES" TO DL-PROD-TYPE
                           WHEN SOR-PROD-CODE(SUB) = "O"
                              MOVE "OILS" TO DL-PROD-TYPE
                           WHEN SOR-PROD-CODE(SUB) = "C"
                                MOVE "CAPSULES" TO DL-PROD-TYPE
                           WHEN SOR-PROD-CODE(SUB) = ' '
                                MOVE SPACES TO DL-PROD-TYPE
                           WHEN OTHER
                                MOVE "BAD" TO DL-PROD-TYPE
								
                      END-EVALUATE 
	                  
			.

            500-NUMBOXVALIDATE.
                 IF SOR-NUM-BOXES(SUB) NOT NUMERIC
                      MOVE 0 TO DL-NUM-SOLD
					  MOVE 0 TO SOR-NUM-BOXES(SUB)
                 ELSE
                      MOVE SOR-NUM-BOXES(SUB) TO DL-NUM-SOLD
						   
                 END-IF

            .

            525-PRICEVALIDATE.
                 IF SOR-PRICE-PER(SUB) NOT NUMERIC
                      MOVE 0 TO DL-BOX-PRICE
					  MOVE 0 TO SOR-PRICE-PER(SUB)
                 ELSE
                      MOVE SOR-PRICE-PER(SUB) TO DL-BOX-PRICE
						   
                 END-IF

            .

            550-MONTHVALIDATE.
                 IF SOR-MONTH-BOUGHT(SUB) NOT NUMERIC
                      MOVE SPACES TO DL-MONTH-BOUGHT
						   
                 ELSE
                      EVALUATE TRUE
                           WHEN SOR-MONTH-BOUGHT(SUB) = 08
                                MOVE 'AUGUST' TO DL-MONTH-BOUGHT
                           WHEN SOR-MONTH-BOUGHT(SUB) = 09
                                MOVE 'SEPTEMBER' TO DL-MONTH-BOUGHT
                           WHEN SOR-MONTH-BOUGHT(SUB) = 10
                                MOVE 'OCTOBER' TO DL-MONTH-BOUGHT
                           WHEN OTHER
                                MOVE 'BAD' TO DL-MONTH-BOUGHT
								
                      END-EVALUATE
                 END-IF

            .

            575-CALCMONTH.
                 MULTIPLY SOR-NUM-BOXES(SUB) BY SOR-PRICE-PER(SUB)
                      GIVING TF-MONTH-VAL
                 MOVE TF-MONTH-VAL TO DL-TOT-SOLD-PER
                 ADD TF-MONTH-VAL TO WSHTEMPMONTOTAL

            .

            600-IDBREAK.
                 MOVE WSHTEMPMONTOTAL TO TL-TOTAL-SOLD
				 ADD WSHTEMPMONTOTAL TO TF-CITY-VAL
				 MOVE 2 TO PROPER-SPACING
				 WRITE REPORT-RECORD FROM TOTAL-LINE
				      AFTER ADVANCING PROPER-SPACING
				 MOVE ZEROES TO WSHTEMPMONTOTAL
				 MOVE ZEROES TO TL-TOTAL-SOLD
				 MOVE SOR-CUST-ID TO CUST-HOLD
				 ADD 2 TO LINE-COUNT
				 
            .

            625-CITYBREAK.
                 PERFORM 600-IDBREAK
				 MOVE TF-CITY-VAL TO CTL-CITY-TOTAL
				 WRITE REPORT-RECORD FROM CITY-TOTAL-LINE
				      AFTER ADVANCING PROPER-SPACING
				 ADD TF-CITY-VAL TO TF-STATE-VAL
				 MOVE ZEROES TO TF-CITY-VAL
				 MOVE SOR-CITY TO WSHTEMPCITY
				 ADD 2 TO LINE-COUNT
				 
            .
			
			650-STATEBREAK.
                 PERFORM 625-CITYBREAK
				 MOVE TF-STATE-VAL TO STL-STATE-TOTAL
				 MOVE 1 TO PROPER-SPACING
				 WRITE REPORT-RECORD FROM STATE-TOTAL-LINE
                      AFTER ADVANCING PROPER-SPACING
                 ADD TF-STATE-VAL TO GT-TOTAL
                 MOVE ZEROES TO TF-STATE-VAL
				 MOVE SOR-STATE TO WSHTEMPSTATE
				 ADD 2 TO LINE-COUNT

            .

            700-CALCGRANDTOTAL.
                 MOVE GT-TOTAL TO GTL-TOTAL
                 MOVE 1 TO PROPER-SPACING
                 WRITE REPORT-RECORD FROM GRAND-TOTAL-LINE
                      AFTER ADVANCING PROPER-SPACING
                 ADD 2 TO LINE-COUNT

            .				 

            900-EOF-ROUTINE.
                 CLOSE SORTED-ORDER-FILE
                       ORDER-REPORT-RECORD

            .				 
				
			
				
						  
				
			
			
			
						
		
				
			
				
			
	   
				
	  
	  
