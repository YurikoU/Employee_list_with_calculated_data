 IDENTIFICATION DIVISION.                                         
 PROGRAM-ID. PROG1.                                               
 AUTHOR. TIM PATRICK.                                             
 * ASSIGNMENT 2.                                                  
 ENVIRONMENT DIVISION.                                            
 CONFIGURATION SECTION.                                           
 INPUT-OUTPUT SECTION.                                            
 FILE-CONTROL.                                                    
     SELECT INPUT-FILE   ASSIGN TO DA-S-INPUT.                    
     SELECT REPORT-FILE  ASSIGN TO UR-S-REPORT.                   
 DATA DIVISION.                                                   
 FILE SECTION.                                                    
 FD INPUT-FILE                                                    
     BLOCK CONTAINS 0 RECORDS                                     
     LABEL RECORDS ARE STANDARD.                                  
 01 INPUT-REC            PIC X(106).                              
 FD REPORT-FILE                                                   
     LABEL RECORDS ARE OMITTED.                                   
 01 PRNT-REC             PIC X(125).                              
 WORKING-STORAGE SECTION.                                         
***********************************************************       
*      LAYOUT FOR THE INPUT FILE                          *       
*      TO STORE EACH VALUE RESPECTIVELY                   *       
***********************************************************       
* READ THE EMPLOYEES DATA AND ASSIGN IT TO NEW VARIABLES          
 01 INPUT-DATA.                                                   
     03 I-EMPID          PIC 9(7).                                
     03 I-LNAME          PIC X(15).                               
     03 I-FNAME          PIC X(15).                               
     03 I-EMPTYPE        PIC 9(2).                                
     03 I-TITLE          PIC X(17).                               
     03 I-SSN.                                                    
        05 I-SSN1        PIC 999.                                 
        05 I-SSN2        PIC 99.                                  
        05 I-SSN3        PIC 9999.                                
     03 FILLER           PIC X(24)    VALUE SPACES.               
     03 I-DATE           PIC 9(8).                                
     03 FILLER           PIC X(2)     VALUE SPACES.               
     03 I-EMPRATE        PIC 9999V99.                             
     03 I-EMPSTATUS      PIC X(1).                                
                                                                  
                                                                  
***********************************************************       
*    LAYOUT FOR THE OUTPUT DATA LINE OF REPORT PRINTING   *       
*    FOR RETRIVED EMPLOYEE DATA                           *       
***********************************************************       
* PRINT THE FORMATTED TABLE BODY USING THE EMPLOYEES DATE         
 01 PRNT-DATA1.                                                   
     03 L-SSN.                                                    
        05 L-SSN1        PIC 999.                                 
        05 DASH          PIC X VALUE '-'.                         
        05 L-SSN2        PIC 99.                                  
        05 DASH          PIC X VALUE '-'.                         
        05 L-SSN3        PIC 9999.                                
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 L-LNAME          PIC X(15).                               
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 L-FNAME          PIC X(15).                               
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 L-EMPID          PIC 9(7).                                
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 L-TITLE          PIC X(17).                               
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 L-EMPTYPE        PIC Z9.                                  
     03 FILLER           PIC X(3)    VALUE SPACES.                
     03 L-DATE           PIC 99/99/9999.                          
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 L-EMPRATE        PIC Z,ZZ9.99.                            
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 L-EMPSTATUS      PIC X(1).                                
                                                                  
                                                                  
************************************************************      
*      LAYOUT FOR THE HEADING LINES OF REPORT PRNTING      *      
*      INCLUDING THE COLUMN NAMES AND TABLE TITLE          *      
************************************************************      
* PRINT THE FIRST LINE OF THE TABLE HEADER                        
 01 PRNT-HEADING1.                                                
     03 REPORT-DATE      PIC 99/99/99.                            
     03                  PIC X(20) VALUE SPACES.                  
     03         PIC X(28) VALUE 'THE BEST IS YET TO COME, INC'.   
     03                  PIC X(15) VALUE SPACES.                  
     03                  PIC X(5) VALUE 'PAGE '.                  
     03 REPORT-PAGE      PIC ZZ9.                                 
* PRINT THE SECOND LINE OF THE TABLE HEADER                       
 01 PRNT-HEADING2.                                                
     03                  PIC X(17) VALUE SPACES.                  
     03         PIC X(31) VALUE 'EMPLOYEE CLASSIFICATION AND PAY'.
* PRINT THE COLUMN NAMES OF THE TABLE                             
 01 PRNT-HEADING3.                                                
     03                  PIC X(3)  VALUE 'SSN'.                   
     03                  PIC X(9)  VALUE SPACES.                  
     03                  PIC X(4)  VALUE 'LAST'.                  
     03                  PIC X(12) VALUE SPACES.                  
     03                  PIC X(5)  VALUE 'FIRST'.                 
     03                  PIC X(11) VALUE SPACES.                  
     03                  PIC X(6)  VALUE 'EMP ID'.                
     03                  PIC X(2)  VALUE SPACES.                  
     03                  PIC X(5)  VALUE 'TITLE'.                 
     03                  PIC X(13) VALUE SPACES.                  
     03                  PIC X(4)  VALUE 'TYPE'.                  
     03                  PIC X(1)  VALUE SPACES.                  
     03                  PIC X(4)  VALUE 'DATE'.                  
     03                  PIC X(7)  VALUE SPACES.                  
     03                  PIC X(4)  VALUE 'RATE'.                  
     03                  PIC X(5)  VALUE SPACES.                  
     03                  PIC X(2)  VALUE 'ST'.                    
 01 MISC.                                                         
                                                                  
                                                                  
************************************************************      
*    LAYOUT FOR THE PAYMENT TENDENCY OF THE EMPLOYEE DATA  *      
*        - THE NUMBER OF HOURLY OR SALARIED EMPLOYEES      *      
*        - THE AVERAGE PAYMENT RATE OF EACH                *      
************************************************************      
* PRINT THE FIRST LINE OF THE PAYMENT ANALYSIS                    
 01 PRNT-PAYMENT1.                                                
     03       PIC X(33) VALUE 'NUMBER OF EMPLOYEE RECORDS READ:'. 
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 REPORT-TOTAL     PIC ZZ9.                                 
* PRINT THE SECOND LINE OF THE PAYMENT ANALYSIS                   
 01 PRNT-PAYMENT2.                                                
     03       PIC X(28) VALUE 'NUMBER OF HOURLY EMPLOYEES:'.      
     03 FILLER           PIC X(6)    VALUE SPACES.                
     03 REPORT-STATUS-H  PIC ZZ9.                                 
     03 FILLER           PIC X(5)    VALUE SPACES.                
     03       PIC X(21) VALUE 'AVERAGE HOURLY RATE:'.             
     03 FILLER           PIC X(6)    VALUE SPACES.                
     03 REPORT-HOURLY-RATE    PIC $Z9.99.                         
     03 FILLER           PIC X(21)    VALUE SPACES.               
* PRINT THE THIRD LINE OF THE PAYMENT ANALYSIS                    
 01 PRNT-PAYMENT3.                                                
     03       PIC X(30) VALUE 'NUMBER OF SALARIED EMPLOYEES:'.    
     03 FILLER           PIC X(4)    VALUE SPACES.                
     03 REPORT-STATUS-S  PIC ZZ9.                                 
     03 FILLER           PIC X(5)    VALUE SPACES.                
     03       PIC X(23) VALUE 'AVERAGE SALARIED RATE:'.           
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 REPORT-SALARIED-RATE  PIC $Z,ZZ9.99.                      
     03 FILLER           PIC X(21)    VALUE SPACES.               
                                                                  
************************************************************      
*    LAYOUT FOR THE NUMBER OF EACH EMPLOYEE TYPE           *      
*     OF THE EMPLOYEE DATA                                 *      
*        - EMPLOYEE TYPE: 1-10                             *      
************************************************************      
* PRINT THE NUMBERS OF EACH EMPLOYEE TYPE, FROM 1 TO 5            
 01 PRNT-NUM-EMPTYPE1.                                            
     03                  PIC X(7)    VALUE 'TYPE 1:'.             
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 OUTPUT-TYPE1     PIC ZZ9.                                 
     03 FILLER           PIC X(5)    VALUE SPACES.                
     03                  PIC X(7)    VALUE 'TYPE 2:'.             
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 OUTPUT-TYPE2     PIC ZZ9.                                 
     03 FILLER           PIC X(5)    VALUE SPACES.                
     03                  PIC X(7)    VALUE 'TYPE 3:'.             
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 OUTPUT-TYPE3     PIC ZZ9.                                 
     03 FILLER           PIC X(5)    VALUE SPACES.                
     03                  PIC X(7)    VALUE 'TYPE 4:'.             
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 OUTPUT-TYPE4     PIC ZZ9.                                 
     03 FILLER           PIC X(5)    VALUE SPACES.                
     03                  PIC X(7)    VALUE 'TYPE 5:'.             
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 OUTPUT-TYPE5     PIC ZZ9.                                 
     03 FILLER           PIC X(5)    VALUE SPACES.                
* PRINT THE NUMBERS OF EACH EMPLOYEE TYPE, FROM 6 TO 10           
 01 PRNT-NUM-EMPTYPE2.                                            
     03                  PIC X(7)    VALUE 'TYPE 6:'.             
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 OUTPUT-TYPE6     PIC ZZ9.                                 
     03 FILLER           PIC X(5)    VALUE SPACES.                
     03                  PIC X(7)    VALUE 'TYPE 7:'.             
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 OUTPUT-TYPE7     PIC ZZ9.                                 
     03 FILLER           PIC X(5)    VALUE SPACES.                
     03                  PIC X(7)    VALUE 'TYPE 8:'.             
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 OUTPUT-TYPE8     PIC ZZ9.                                 
     03 FILLER           PIC X(5)    VALUE SPACES.                
     03                  PIC X(7)    VALUE 'TYPE 9:'.             
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 OUTPUT-TYPE9     PIC ZZ9.                                 
     03 FILLER           PIC X(5)    VALUE SPACES.                
     03                  PIC X(8)    VALUE 'TYPE 10:'.            
     03 OUTPUT-TYPE10    PIC ZZ9.                                 
     03 FILLER           PIC X(50)    VALUE SPACES.               
                                                                  
                                                                  
************************************************************      
*      END OF FILE (EOF) SWITCHES                          *      
*    0=NOT AT EOF       1=AT EOF                           *      
*                                                          *      
*    DECLARE VARIABLES FOR CALCULATION AS BELOW            *      
*        - PRINT TODAY'S DATE                              *      
*        - COUNT THE ROWS OF THE TABLE                     *      
*        - CALCULATE THE AVERAGE PAYMENT                   *      
************************************************************      
     03 EOF-I            PIC 9       VALUE 0.                     
                                                                  
* VARIABLE TO STORE THE DATE                                      
     03 CURRENT-DATE     PIC 999999.                              
                                                                  
* VARIABLES TO PRINT THE PAGE UMBER AND COUNT THE TABLE ROWS      
     03 PAGE-NUMBER      PIC 999     VALUE 0.                     
     03 LINE-COUNT       PIC 99      VALUE 0.                     
     03 TOTAL-RECORDS    PIC 999 VALUE ZEROS.                     
                                                                  
* VARIABLES TO COUNT EACH EMPLOYEE TYPE                           
     03 TYPE-COUNT OCCURS 10 TIMES PIC 999 VALUE ZEROS.           
     03 COUNT-S          PIC 999 VALUE 0.                         
     03 COUNT-H          PIC 999 VALUE 0.                         
                                                                  
* VARIABLES FOR THE PAYMENT CALCULATION                           
     03 HOURLY-RATE      PIC 99V99 VALUE ZEROS.                   
     03 SALARIED-RATE    PIC 9999V99 VALUE ZEROS.                 
     03 HOURLY-TOTAL     PIC 99999V99 VALUE ZEROS.                
     03 SALARIED-TOTAL   PIC 99999V99 VALUE ZEROS.                
                                                                  
                                                                  
************************************************************      
*      START OF PROCEDURE DIVISION                         *      
************************************************************      
 PROCEDURE DIVISION.                                              
                                                                  
                                                                  
************************************************************      
* THE MAINLINE IS RESPONSIBILE FOR THE FLOW OF THE LOGIC   *      
* ALL MAIN PROCEDURES SHOULD BE CALLED FROM THE MAIN       *      
* EVERY PROCEDURE (PARAGRAPH) MUST BE DOCUMENTED           *      
************************************************************      
 000-MAINLINE.                                                    
     OPEN INPUT INPUT-FILE                                        
          OUTPUT REPORT-FILE.                                     
     PERFORM 9000-READ-INPUT.                                     
     PERFORM 5000-PRINT-HEAD.                                     
     PERFORM 1000-LOOP                                            
         UNTIL EOF-I = 1.                                         
     PERFORM 6000-PRINT-PAYMENT.                                  
     PERFORM 7000-PRNT-NUM-EMPTYPE.                               
     CLOSE INPUT-FILE                                             
         REPORT-FILE.                                             
     STOP RUN.                                                    
                                                                  
                                                                  
************************************************************      
*      1000-LOOP CALLS 1600-PRINT-NAMES WHICH IS           *      
*      RESPONSIBLE FOR MOVING DATA TO PRINT LINE           *      
*      AND THEN PRINTING                                   *      
*      IT NEXT CALLS 9000-READ-INPUT WHICH WILL READ       *      
*      THE NEXT RECORD INTO THE STRUCTURE FOR PROCESSING   *      
************************************************************      
* HANDLE THE TABLE BODY PART                                      
 1000-LOOP.                                                       
     PERFORM 1600-PRINT-NAMES.                                    
     PERFORM 2000-COUNT-EMPTYPE.                                  
     PERFORM 3000-COUNT-HS.                                       
     PERFORM 9000-READ-INPUT.                                     
                                                                  
                                                                  
************************************************************      
*      1600-PRINT-NAMES WILL MOVE NECESSARY FIELDS TO THE  *      
*      PRINT STRUCTURE IN WORKING-STORAGE ASD THEN IT WILL *      
*      PRINT THE INFORMATION                               *      
*                                                          *      
*      IF IT READS 10 ROWS, START THE NEXT PAGE AND        *      
*      PRINT THE FOLLOWING ROWS AFTER THE HEADLINE         *      
************************************************************      
 1600-PRINT-NAMES.                                                
* ONCE IT READS 10 ROWS, RESET THE COUNTER AND START NEW PAGE     
     COMPUTE LINE-COUNT = LINE-COUNT + 1                          
     IF (10 < LINE-COUNT) THEN                                    
         PERFORM 5000-PRINT-HEAD                                  
         COMPUTE LINE-COUNT = 1                                   
     END-IF                                                       
                                                                  
* COPY AND PASTE THE VARIABLES TO PRINT                           
     MOVE I-EMPID         TO L-EMPID                              
     MOVE I-LNAME         TO L-LNAME                              
     MOVE I-FNAME         TO L-FNAME                              
     MOVE I-EMPTYPE       TO L-EMPTYPE                            
     MOVE I-TITLE         TO L-TITLE                              
     MOVE I-SSN1          TO L-SSN1                               
     MOVE I-SSN2          TO L-SSN2                               
     MOVE I-SSN3          TO L-SSN3                               
     MOVE I-DATE          TO L-DATE                               
     MOVE I-EMPRATE       TO L-EMPRATE                            
     MOVE I-EMPSTATUS     TO L-EMPSTATUS                          
                                                                  
* PRINT THE RETRIEVED DATA ON THE TABLE BODY                      
     WRITE PRNT-REC FROM PRNT-DATA1                               
           AFTER ADVANCING 1 LINE.                                
                                                                  
************************************************************      
*      2000-COUNT-EMPTYPE COUNTS THE NUMBERS OF            *      
*      EACH EMPLOYEE TYPE TO PRINT THEM ON THE FOOTER      *      
************************************************************      
 2000-COUNT-EMPTYPE.                                              
* COUNT THE NUM OF EMPLOYEES BY ADDING 1 EVERY EMPLOYEE           
     ADD 1 TO TOTAL-RECORDS                                       
     ADD 1 TO TYPE-COUNT(I-EMPTYPE)                               
                                                                  
* COUNT THE PROPER OUTPUT-TYPE BASED ON THE VAL OF I-EMPTYPE      
     MOVE TYPE-COUNT(1)   TO OUTPUT-TYPE1                         
     MOVE TYPE-COUNT(2)   TO OUTPUT-TYPE2                         
     MOVE TYPE-COUNT(3)   TO OUTPUT-TYPE3                         
     MOVE TYPE-COUNT(4)   TO OUTPUT-TYPE4                         
     MOVE TYPE-COUNT(5)   TO OUTPUT-TYPE5                         
     MOVE TYPE-COUNT(6)   TO OUTPUT-TYPE6                         
     MOVE TYPE-COUNT(7)   TO OUTPUT-TYPE7                         
     MOVE TYPE-COUNT(8)   TO OUTPUT-TYPE8                         
     MOVE TYPE-COUNT(9)   TO OUTPUT-TYPE9                         
     MOVE TYPE-COUNT(10)  TO OUTPUT-TYPE10.                       
                                                                  
************************************************************      
*      3000-COUNT-HS COUNTS THE NUMBERS OF                 *      
*      HOURLY OR SALARIED EMPLOYEES TO PRINT THEM          *      
*      ON THE FOOTERF, AND CALCULATES THE TOTAL PAYMENT    *      
************************************************************      
 3000-COUNT-HS.                                                   
* COUNT EACH EMPLOYEE STATUS AND THE TOTAL PAYMENT                
     IF (I-EMPSTATUS = 'H') THEN                                  
         ADD 1 TO COUNT-H                                         
         COMPUTE HOURLY-TOTAL = HOURLY-TOTAL + I-EMPRATE          
     ELSE                                                         
         ADD 1 TO COUNT-S                                         
         COMPUTE SALARIED-TOTAL = SALARIED-TOTAL + I-EMPRATE      
     END-IF                                                       
                                                                  
* COPY AND PASTE THE VARIABLES TO PRINT                           
     MOVE COUNT-H         TO REPORT-STATUS-H                      
     MOVE COUNT-S         TO REPORT-STATUS-S.                     
                                                                  
************************************************************      
*      5000-PRINT-HEAD PRINTS A HEADER LINE                *      
*      AFTER IT MOVES TO A NEW PAGE, INCLUDING TODAY'S     *      
*      DATE AND THE PAGE NUMBER OF EACH PAGE               *      
************************************************************      
 5000-PRINT-HEAD.                                                 
* CALL DATE FUNCTION AND ASSIGN IT TO THE VARIABLE                
     ACCEPT CURRENT-DATE FROM DATE                                
     MOVE CURRENT-DATE    TO REPORT-DATE                          
                                                                  
* COUNT THE PAGE NUMBER BY ADDING 1 EVERY NEW PAGE                
     COMPUTE PAGE-NUMBER = PAGE-NUMBER + 1                        
     MOVE PAGE-NUMBER     TO REPORT-PAGE                          
                                                                  
* PRINT THE TABLE HEADER                                          
* ADD EXTRA LINES BEFORE EACH PAGE EXCEPT THE FIRST PAGE          
     IF (1 < PAGE-NUMBER) THEN                                    
         MOVE SPACES TO PRNT-REC                                  
         WRITE PRNT-REC                                           
             AFTER ADVANCING 1 LINE                               
         WRITE PRNT-REC                                           
             AFTER ADVANCING 1 LINE                               
     END-IF                                                       
                                                                  
     WRITE PRNT-REC FROM PRNT-HEADING1                            
           AFTER ADVANCING 2 LINE                                 
     WRITE PRNT-REC FROM PRNT-HEADING2                            
           AFTER ADVANCING PAGE                                   
     WRITE PRNT-REC FROM PRNT-HEADING3                            
           AFTER ADVANCING PAGE                                   
     MOVE SPACES TO PRNT-REC                                      
     WRITE PRNT-REC                                               
           AFTER ADVANCING 1 LINE.                                
                                                                  
************************************************************      
*      6000-PRINT-PAYMENT CALCULATES THE AVERAGE PAYMENT   *      
*      AND PRINTS IT ON THE TABLE FOOTER                   *      
*          - THE NUMBER OF EMPLOYEES                       *      
*          - THE NUMBER OF EMPLOYEES PER EMPLOYEE STATUS   *      
*          - THE AVERAGE PEYMENT RATE PER EMPLOYEE STATUS  *      
************************************************************      
 6000-PRINT-PAYMENT.                                              
* CALCULATE THE AVERAGE PAYMENT PER EMPLOYEE STATUS               
     COMPUTE HOURLY-RATE = HOURLY-TOTAL / COUNT-H                 
     COMPUTE SALARIED-RATE = SALARIED-TOTAL / COUNT-S             
     MOVE TOTAL-RECORDS   TO REPORT-TOTAL                         
     MOVE HOURLY-RATE     TO REPORT-HOURLY-RATE                   
     MOVE SALARIED-RATE   TO REPORT-SALARIED-RATE                 
                                                                  
* PRINT THE PAYMENT TENDENCY ON THE TABLE FOOTER                  
     MOVE SPACES TO PRNT-REC                                      
     WRITE PRNT-REC                                               
           AFTER ADVANCING 1 LINE                                 
     WRITE PRNT-REC FROM PRNT-PAYMENT1                            
           AFTER ADVANCING PAGE                                   
     WRITE PRNT-REC FROM PRNT-PAYMENT2                            
           AFTER ADVANCING PAGE                                   
     WRITE PRNT-REC FROM PRNT-PAYMENT3                            
           AFTER ADVANCING PAGE                                   
     MOVE SPACES TO PRNT-REC                                      
     WRITE PRNT-REC                                               
           AFTER ADVANCING 1 LINE.                                
                                                                  
************************************************************      
*      7000-PRNT-NUM-EMPTYPE PRINTS THE NUMBER OF EACH     *      
*      EMPLOYEE TYPE AT THE VERY LAST OF THE TABLE         *      
************************************************************      
 7000-PRNT-NUM-EMPTYPE.                                           
* PRINT THE NUMBER OF EMPLOYEE TYPES ON THE TABLE FOOTER          
     WRITE PRNT-REC FROM PRNT-NUM-EMPTYPE1                        
           AFTER ADVANCING PAGE                                   
     WRITE PRNT-REC FROM PRNT-NUM-EMPTYPE2                        
           AFTER ADVANCING PAGE                                   
     MOVE SPACES TO PRNT-REC                                      
     WRITE PRNT-REC                                               
           AFTER ADVANCING 1 LINE.                                
                                                                  
                                                                  
************************************************************      
*      9000-READ-INPUT READS A RECORD AT A TIME                   
*      THE RECORD IS READ INTO THE STRUCTURE SET UP IN            
*      WORKING STORAGE                                            
************************************************************      
 9000-READ-INPUT.                                                 
* READ THE ORIGINAL EMPLOYEE DATA                                 
     READ INPUT-FILE INTO INPUT-DATA                              
          AT END MOVE 1 TO EOF-I.                                 