{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version.Version14 (sql) where

import Data.String.Interpolate

sql :: String 
sql = [i|drop table "edgeNode"."QualificationProvider" cascade;
         create table "edgeNode"."QualificationProvider" 
         (id bigserial primary key,
         "qualificationProviderKey" int8 not null 
         constraint "QualificationProvider_provider_id_fk" 
         references "edgeNode"."Provider"(id),
         "qualificationProviderDegreeType" text,
         "qualificationProviderTitle" text not null,
         "qualificationProviderGradeRange" jsonb, path ltree,
         constraint "QualificationProvider_qualificationProviderKey_qualificationProviderTitle_path_uniq" 
         unique ("qualificationProviderKey", "qualificationProviderTitle", path));
         insert into "edgeNode"."QualificationProvider" 
         ("qualificationProviderTitle", 
          "qualificationProviderDegreeType", 
          "qualificationProviderKey", path, 
          "qualificationProviderGradeRange") 
          select 'Literature', 'UnifiedStateExam', 5, 'Literature', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('integral', i)), 'rank', i))) 
          from generate_series(1, 100) as i;
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Social Theory', 'UnifiedStateExam', 5, 'SocialTheory', 
           array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('integral', i)), 'rank', i))) 
           from generate_series(1, 100) as i;          
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Mathematics', 'UnifiedStateExam', 5, 'Mathematics', 
           array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('integral', i)), 'rank', i))) 
           from generate_series(1, 100) as i;
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'History', 'UnifiedStateExam', 5, 'History', 
           array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('integral', i)), 'rank', i))) 
           from generate_series(1, 100) as i;
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Biology', 'UnifiedStateExam', 5, 'Biology', 
           array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('integral', i)), 'rank', i))) 
           from generate_series(1, 100) as i;
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Physics', 'UnifiedStateExam', 5, 'Physics', 
           array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('integral', i)), 'rank', i))) 
           from generate_series(1, 100) as i;
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Robotics', 'MRes', 1, 'Robotics', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2, 3, 4], array['I', 'II-1', 'II-2', 'III']) as tpl(i, x);
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Law', 'LLB', 1, 'Law', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2, 3, 4], array['I', 'II-1', 'II-2', 'III']) as tpl(i, x);
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Physics', 'MSc', 1, 'Physics', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2, 3, 4], array['I', 'II-1', 'II-2', 'III']) as tpl(i, x);
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Experimental Psychology', 'MPhil', 1, 'ExperimentalPsychology', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2, 3, 4], array['I', 'II-1', 'II-2', 'III']) as tpl(i, x);
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Fine Art', 'BFA', 1, 'FineArt', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2, 3, 4], array['I', 'II-1', 'II-2', 'III']) as tpl(i, x);
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Classics', 'BA', 1, 'Classics', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2, 3, 4], array['I', 'II-1', 'II-2', 'III']) as tpl(i, x);
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Architecture', 'BSc', 1, 'Architecture', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2, 3, 4], array['I', 'II-1', 'II-2', 'III']) as tpl(i, x);
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Applied Social Psychology', 'Magistr', 4, 'AppliedSocialPsychology', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2], array['Std-dip', 'Out-dip']) as tpl(i, x);                                       
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Public Law', 'Magistr', 4, 'PublicLaw', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2], array['Std-dip', 'Out-dip']) as tpl(i, x);
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Big Data Systems', 'Magistr', 4, 'BigDataSystems', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2], array['Std-dip', 'Out-dip']) as tpl(i, x);
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Applied Mathematics & Computing', 'Bakalavr', 4, 'AppliedMathematicsComputing', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2], array['Std-dip', 'Out-dip']) as tpl(i, x); 
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Philosophy', 'Bakalavr', 4, 'Philosophy', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2], array['Std-dip', 'Out-dip']) as tpl(i, x);
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Design', 'Bakalavr', 4, 'Design', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2], array['Std-dip', 'Out-dip']) as tpl(i, x);
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Mathematics', 'ALevelGCE', 2, 'Mathematics', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2, 3, 4, 5, 6], array['A*', 'A', 'B', 'C', 'D', 'F']) as tpl(i, x);
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Religious Studies', 'ALevelGCE', 2, 'ReligiousStudies', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2, 3, 4, 5, 6], array['A*', 'A', 'B', 'C', 'D', 'F']) as tpl(i, x);
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Economics', 'ALevelGCE', 2, 'Economics', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2, 3, 4, 5, 6], array['A*', 'A', 'B', 'C', 'D', 'F']) as tpl(i, x);
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'French', 'ALevelGCE', 3, 'French', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2, 3, 4, 5, 6], array['A*', 'A', 'B', 'C', 'D', 'F']) as tpl(i, x);
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'Physics A', 'ALevelGCE', 3, 'PhysicsA', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2, 3, 4, 5, 6], array['A*', 'A', 'B', 'C', 'D', 'F']) as tpl(i, x);
          insert into "edgeNode"."QualificationProvider" 
          ("qualificationProviderTitle", 
           "qualificationProviderDegreeType", 
           "qualificationProviderKey", path, 
           "qualificationProviderGradeRange") 
          select 'PhysicsB', 'ALevelGCE', 3, 'PhysicsB', 
          array_to_json(array_agg(json_build_object('grade', json_build_object('value', json_build_object('text', x)), 'rank', i))) 
          from unnest(array[1, 2, 3, 4, 5, 6], array['A*', 'A', 'B', 'C', 'D', 'F']) as tpl(i, x);|]