{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version.Version19 (sql) where

import Data.String.Interpolate

sql :: String 
sql = [i|insert into "edgeNode"."StateExam" 
         ("stateExamTitle", "stateExamCountry") 
         values ('Unified State Exam', 'russia');
         insert into "edgeNode"."StateExam" 
         ("stateExamTitle", "stateExamCountry") 
         values ('Advanced Level (GCE)', 'united_kingdom');        
         insert into "edgeNode"."StateExam" 
         ("stateExamTitle", "stateExamCountry") 
         values ('Cambridge (Pre-U)', 'united_kingdom');
         insert into "edgeNode"."InternationalDiploma"
         ("internationalDiplomaTitle") values ('International Baccalaureate (IB)');
         insert into "edgeNode"."HigherDegree" ("higherDegreeCountry", "higherDegreeProvider")
         values ('russia', 'Higher School of Economics'); 
         insert into "edgeNode"."HigherDegree" ("higherDegreeCountry", "higherDegreeProvider")
         values ('united_kingdom', 'University College London');
         insert into "edgeNode"."LanguageStandard" 
         ("languageStandardStandard", "languageStandardGrade")
         values ('IELTS Academic', '{"intgeral":0,"fractional":0}'); 
         insert into "edgeNode"."LanguageStandard" 
         ("languageStandardStandard", "languageStandardGrade")
         values ('TOEFL', '{"intgeral":0,"fractional":0}');|]