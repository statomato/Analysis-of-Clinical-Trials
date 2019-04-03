TITLE1 '*** sqlug1: User guide chapter 1 examples ***';

DATA employee;
  INPUT empnum empname $ empyears empcity $ 20-34
  emptitle $ 36-45 empboss;
CARDS;
101  Herb  28       Ocean City      president  .
201  Betty  8        Ocean City       manager  101
213  Joe  2         Virginia Beach   salesrep   201
214  Jeff  1        Virginia Beach   salesrep  201
215  Wanda  10     Ocean City       salesrep  201
216  Fred  6        Ocean City       salesrep  201
301  Sally  9        Wilmington       manager  101
314  Marvin  5      Wilmington       salesrep  301
318  Nick  1        Myrtle Beach     salesrep  301
401  Chuck  12      Charleston       manager  101
417  Sam  7         Charleston       salesrep  401
;
RUN;

/*21.2 BASE SAS System의 SQL PROCEDURE*/
PROC SQL;
  TITLE2 'CITY AND YEARS of SERVICE' ;

  SELECT empname, empcity, empyears
  FROM employee
  WHERE emptitle = 'salesrep';

/*21.3 SQL PROCEDURE와 SAS DATA Step의 비교*/
TITLE2 'TOTAL SERVICE YEARS';
TITLE3 'Computed with PROC SQL';

SELECT empcity, SUM(empyears) AS totyears
  FROM employee
  WHERE emptitle = 'salesrep'
  GROUP BY empcity
  ORDER BY totyears;

  /*(2)*/
TITLE2 'TOTAL SERVICE YEARS';
TITLE3 'Computed with PROCS SUMMARY, SORT AND PRINT';

PROC SUMMARY DATA=employee;
  WHERE emptitle= 'salesrep';
  CLASS empcity;
  VAR empyears;
  OUTPUT OUT=sumyears SUM=totyears;
RUN;
PROC SORT DATA=sumyears;
  BY totyears;
RUN;

PROC PRINT DATA=sumyears NOOBS;
  VAR empcity totyears;
  WHERE _type_=1;
RUN;

/*21.4 VIEW의 생성(CREATE문)*/
PROC SQL;
  TITLE2 'EMPLOYEES WHO RESIDE IN OCEAN CITY';

  CREATE VIEW ocity AS
  SELECT empname, empcity, emptitle, empyears
  FROM employee
  WHERE empcity = 'Ocean City';

PROC PRINT DATA = ocity;
  SUM empyears;
  RUN;

/*22.1 Data의 입력*/
DATA paper;
 INPUT author$1-8 sections$ 9-16 title$17-43 @45 time TIME5. @52 duration; 
 FORMAT time TIME5.;
 LABEL title = 'Paper Title'; 
 CARDS;
Tom      Testing  Automated Product Testing   9:00   35
Jerry   Testing Involving Users             9:50    30
Nick     Testing Plan to test, test to plan  10:30 20
Peter    Info     SysArtificial Intelligence9:30   45
Paul      Info    SysQuery Languages         10:30  40
Lewis     Info    SysQuery Optimisers        15:30  25
Jonas   Users   Starting a Local User Group 14:30  35
Jim     Users   Keeping power users happy   15:15  20
Janet   Users   Keeping everyone informed   15:45  30
Marti   Graphics Multi-dimensional graphics  16:30  35
Marge   Graphics Make your own point!       15:10  35
Mike    Graphics Making do without color    15:50  15
Jane    Graphics Primary colors, use em!    16:15  25
Jost               Foreign Language Issues  11:15   .
;

PROC SQL;
TITLE2 'Papers to be presented';
SELECT *
  FROM paper;

PROC SQL;
TITLE2 'How long will it take?';
SELECT author, title, time, duration,
  time + duration*60 AS endtime
  FROM paper;

PROC SQL;
SELECT author, title, time, duration LABEL='HOW Long it Takes',
  time + duration*60 AS endtime FORMAT=TIME5.
  FROM paper;

PROC SQL;
TITLE2 'Papers presented in the morning';
SELECT author, title, time, duration LABEL= 'HowLong it Takes',
  time + duration*60 AS endtime FORMAT=TIME5. FROM paper
  WHERE time < '12:00't;
