module Tests where

import Control.Monad
import Data.Maybe

import Data.DRS

import PGF
import Smallwood
import Representation
import Evaluation
import Model
import WordsCharacters

-- handler gr core tests = putStr $ unlines $ map (\(x,y) -> x++show y) $ zip (map (++"\t") tests ) ( map (\string -> map (\x -> core ( x) ) (parse gr (mkCId "DicksonEng") (startCat gr) string)) tests )

-- import System.Environment.FindBin

ans tests = do
  gr	<- readPGF ( "./Smallwood.pgf" )
  let ss = map (chomp . lc_first) tests
  let ps = map ( parses gr ) ss
  let ls = map (map ( (linear gr) <=< transform ) ) ps
  let zs = zip (map (++"\t") tests) ls
  putStrLn (unlines (map (\(x,y) -> x ++ (show $ unwords (map displayResult y))) zs) )

displayResult = fromMaybe "Nothing"

trans tests = do
  gr	<- readPGF ( "./Smallwood.pgf" )
  let ss = map (chomp . lc_first) tests
  let ps = map ( parses gr ) ss
  let ls = map id ps
  let zs = zip (map (++"\t") tests) (map (map (showExpr []) ) ps)
  putStrLn (unlines (map (\(x,y) -> x ++ (show y ) ) zs) )

reps tests = do
  gr	<- readPGF ( "./Smallwood.pgf" )
  let ss = map (chomp . lc_first) tests
  let ps = map ( parses gr ) ss
  let ts = map (map (\x -> (((unmaybe . rep) x) (term2ref drsRefs var_e) ))) ps
  let zs = zip (map (++"\t") tests) ts
  putStrLn (unlines (map (\(x,y) -> x ++ (show y ) ) zs) )

lf tests = do
	gr	<- readPGF ( "./Smallwood.pgf" )
	let ss = map (chomp . lc_first) tests
	let ps = map ( parses gr ) ss
	let ts = map (map (\p -> drsToLF (((unmaybe . rep) p) (DRSRef "r1"))) ) ps
	let zs = zip (map (++"\t") tests) ts
	putStrLn (unlines (map (\(x,y) -> x ++ (show y ) ) zs) )

fol tests = do
	gr	<- readPGF ( "./Smallwood.pgf" )
	let ss = map (chomp . lc_first) tests
	let ps = map ( parses gr ) ss
	let ts = map (map (\p -> drsToFOL ( (unmaybe . rep) p (term2ref drsRefs var_e) ) ) ) ps
	let zs = zip (map (++"\t") tests) ts
	putStrLn (unlines (map (\(x,y) -> x ++ (show y ) ) zs) )

dic_test = [

  "All the girls Tia goes to high school with talk about being teachers."
  , "When Tia goes to college she starts to study things she really loves."
  , "When Tia goes to college, she starts taking finance and accounting courses."
  , "Tia has a miserable old man for second year accounting and business law"
  , "A miserable old man says to Tia, \"Ms. Casciato, you are the only woman who has ever gotten this far in her class. And she will make sure every day is a living hell for you.\""
   , "Mr Payne grades Tia on her class participation."
  , " Mr Payne grades Tia on how she answers questions."
  , "Mr Payne says to Tia at the beginning of every class, \"I hope you are prepared, Ms. Casciato, because the most difficult question of the period will be yours.\""
  , "Tia graduates from Douglass."
  , "Tia has to go on many job interviews."
  , "Tia thinks she writes 80 letters."
  , "Tia doesn't know how many jobs she applies to."
  , "Tia goes in for the first interview."
  , "Tia owns one dress."
  , "Tia's dress is shades of red and pink."
  , "Tia's dress has big block geometric squares."
  , "Tia's dress has short sleeves."
  , "Tia's dress is a mini-dress."
  , "Tia has tights and heels on."
  , "Tia.walks in."
  , "Mr Batchelor interviews Tia for 15 minutes."
  , "Mr Batchelor says, \"You need to stand up and turn around.\""
  , "Tia says, \"What are you talking about?\""
  , "Mr Batchelor says, \"Stand up and turn around.\""
  , "Tia stands up and leans over Mr Batchelor's desk."
  , "Tia says, \"I don't need this job this much.\""
  , "Mr Batchelor says, \"You're hired.\""

  ]

sekuhara_test = [
	"Christine angered Tia.",
	"Tia offended the company.",
	"Mr Batchelor angered Tia.",
	"Some offensive comments angered Tia.",
	"Mr Batchelor's comments angered Tia.",
	"Mr Batchelor's offensive comments angered Tia.",
	"The comments angered interviewees",
	"Some comments angered interviewees",
	"Some offensive comments angered interviewees",
	"The boss angered visitors.",
	"The company had interviewees.",
	"The boss's comments angered the visitor."
	-- "The company's language angered visitors."
	]


biz_test = [
	"Christine angered Tia.",
	"Tia offended the company.",
	"Mr Batchelor angered Tia.",
	"The comments angered the visitor.",
	"Some comments angered the visitor.",
	"The comments angered visitors.",
	"The comments angered interviewees.",
	"The boss angered visitors.",
	-- "The company's language angered visitors."
	"The company had interviewees.",
	"A company had interviewees."
	]


test_talk = [
	"Mr Batchelor had an offensive request.",
	"Mr Batchelor had an offensive.",
	"Mr Batchelor had an offensive comment.",
	"Mr Batchelor had  offensive comment.",
	"Mr Batchelor had  offensive comments.",
	"Mr Batchelor had  some offensive comments.",
	"Mr Payne had  some offensive comments.",
	"Christine's mother spoke to the boss.",
	"Christine's mother spoke to the interviewer.",
	"Christine's mother talked to the interviewer.",
	"Tia talked to the interviewer.",
	"Tia talked to an interviewer."
	]

test_spell = [
	"Christine could spell names.",
	"Mr Batchelor's daughter could spell names.",
	"Mr Batchelor could spell names.",
	"Mr Batchelor couldn't spell names.",
	"Mr Batchelor knew English but Mr Batchelor couldn't spell names.",
	"Mr Batchelor's boss could spell names.",
	"Mr Batchelor couldn't spell Tia's name.",
	"Christine could spell Mr Batchelor's name,\
	\ but Mr Batchelor couldn't spell Christine's name.",
	"Tia could spell Tia's name and Tia could spell Mr Batchelor's name,\
	\ but Mr Batchelor couldn't spell Tia's name.",
	"Tia could spell names.\
	\ Tia could spell names.\
	\ Mr Batchelor's daughter could spell names.\
	\ But, Mr Batchelor couldn't spell names."
	]

test_text = [
	"Tia's daughter had a brother and Tia's brother had a mother",
	"Christine's mother spoke to the boss but the boss had an offensive request.",
	"Mr Batchelor talked to the boss and the boss talked to Mr Batchelor's daughter.",
	"Mr Batchelor talked to the boss and the boss talked to Tia \
	\and Tia talked to Mr Batchelor.",
	"Tia looked back on Christine's upbringing. \
	\Christine talked to Tia. \
	\Tia's daughter asked Tia about Rutgers University."
	]

test_possessives = [
	"Tia's daughter looked back.",
	"Mr Batchelor's daughter looked back.",
	"Tia's daughter looked back on Mr Batchelor's upbringing.",
	"Tia's daughter looked back on Tia's upbringing.",
	"Mr Batchelor's daughter looked back on Mr Batchelor's upbringing.",
	"The interviewee's dress offended Mr Batchelor.",
	"The interviewer's comments  offended the interviewee.",
	"Did Mr Batchelor look at the woman's red dress?",
	"Did the sister of Mr Batchelor's daughter know English?",
	"Did the father of Mr Batchelor's daughter look back on Mr Batchelor's daughter's upbringing?",
	"Did the brother of Mr Batchelor's daughter look back on Mr Batchelor's daughter's upbringing?",
	"Did the mother of Mr Batchelor's daughter speak English?",
	"Did the father of Mr Batchelor's daughter speak English?",
	"Did the brother of Mr Batchelor's daughter speak English?",
	"Did the mother of Mr Batchelor's daughter speak English?",
	"Did the brother of Mr Batchelor's daughter know English?",
	"Did the sister of Mr Batchelor's daughter know English?"
	]

haves = [
	"Did Mr Batchelor have Mr Batchelor's father?",
	"Did Mr Batchelor have Tia?",
	"Did Mr Batchelor have a mother?",
	"Did Mr Batchelor have a daughter?",
	"Did Mr Batchelor have a daughter?",
	"Did Mr Batchelor's father have a mother?",
	"Did Mr Batchelor have a job?",
	"Did Mr Batchelor have some job?",
	"Did Mr Batchelor have money?",
	"Did Mr Batchelor's father have money?",
	"Did Tia have money?",
	"Did Mr Batchelor have a parent?",
	"Did Mr Batchelor have some parents?",
	"Did Mr Batchelor have parents?",
	"Did Mr Batchelor's father have a parent?",
	"Did Mr Batchelor's father have some parents?",
	"Did Mr Batchelor's father have parents?",
	"Did Mr Batchelor have work?",
	"Did Tia work?",
	"Did Tia have work?",
	"Did Tia have a job?",
	"Did Mr Batchelor's father have work?",
	"Did the boss have the job?",
	"Did the boss have Mr Batchelor's daughter?",
	"Did the boss have a mother?",
	"Did the boss have a daughter?",
	"Did the boss have a daughter?",
	"Did the job have a mother?",
	"Did the boss have comments?",
	"Did the boss have some comments?",
	-- "Did the boss's daughter have some language?",
	-- "Did the boss's daughter have a language?",
	"Did the daughter have some comments?",
	"Did the daughter have no comments?",
	-- "Mr Batchelor's daughter had many language in the Dominican Republic.",
	"Did the parent have some comments?",
	"Did the parent have no comments?",
	"Did the boss have money?",
	"Did Tia have money?",
	"Did the job have money?",
	"Did Mr Batchelor's daughter have money?",
	"Did the boss have a parent?",
	"Did the boss have some parents?",
	"Did the boss have parents?",
	"Did the job have a parent?",
	"Did the job have some parents?",
	"Did the job have parents?",
	"Did the boss have a worker?",
	"Did Mr Batchelor's daughter have a worker?",
	"Did the job have a worker?",
	"Did someone have a worker?"
	]

ungrammatical = [
	"Did Tia worked?",
	"Mr Batchelor work?",
	"Man worked.",
	"Some man work.",
	"No worked.",
	"No-one work.",
	"Did Tia teach?",
	"Tia teach Mr Batchelor.",
	"Mr Batchelor taught.",
	"Did Mr Batchelor's daughter looked back?",
	"the boss look back?",
	"Man looked back.",
	"Some man work.",
	"No looked back.",
	"No-one work.",
	"Did Mr Batchelor's daughter teach?",
	"Mr Batchelor's daughter teach the boss.",
	"the boss raised."
	]

intransitives = [
	"Did Tia work?",
	"Did Mr Batchelor work?",
	"Did Mr Batchelor's father work?",
	"A man worked.",
	"Some man worked.",
	"No one worked.",
	"No-one worked.",
	"Everybody worked.",
	"Everyone worked.",
	-- "Many persons worked.",
	"No person worked.",
	"Did the man work?",
	"Did some man work?",
	"Did some men work?",
	"Did some woman work?",
	"Did some women work?",
	"Most women worked.",
	"Most women didn't work.",
	"Several women worked.",
	"Several women didn't work.",
	"Many women worked.",
	"Many women didn't work.",
	"All women worked.",
	"No woman worked.",
	"Most men worked.",
	"Most men didn't work.",
	"Several men worked.",
	"Several men didn't work.",
	"Many men worked.",
	"Many men didn't work.",
	"All men worked.",
	"No man worked.",
	"Did Mr Batchelor work at a company?",
	"Did Tia work at a company?",
	"Did Tia work at a hospital?",
	"Did Mr Batchelor's father work at a company?",
	"Mr Batchelor's father worked on a company?",
	"Mr Batchelor's father worked in a company?",
	"Mr Batchelor's daughter got married.",
	"Did Mr Batchelor's daughter leave?",
	"Did the boss leave?",
	"Did Mr Batchelor's father leave?",
	"A man left.",
	"Some man left.",
	"No one left.",
	"No-one left.",
	"Everybody left.",
	"Everyone left.",
	"Many persons left.",
	"No person left.",
	"Did the man leave?",
	"Did some man leave?",
	"Did some men leave?",
	"Did some woman leave?",
	"Did some women leave?",
	"Most persons left.",
	"Most men left.",
	"Most men didn't leave.",
	"Several men left.",
	"Several men didn't leave.",
	"Several persons left.",
	"Several persons didn't leave.",
	"Did Mr Batchelor's daughter look back?",
	"Did the boss look back?",
	"Did Mr Batchelor look back?",
	"A man looked back.",
	"Some man looked back.",
	"No one looked back.",
	"No-one looked back.",
	"Everybody looked back.",
	"Everyone looked back.",
	"Many persons looked back.",
	"No person looked back.",
	"Did the man look back?",
	"Did the girl look back?",
	"Did some man look back?",
	"Did some men look back?",
	"Did some woman look back?",
	"Did some women look back?",
	"Most men looked back.",
	"Most men didn't look back.",
	"Several men looked back.",
	"Several men didn't look back.",
	"Many men looked back.",
	"Many men didn't work.",
	"All men looked back.",
	"No man looked back."
	]

transitives = [
	-- "Did Tia study?",
	"Did Tia study IT?",
	"Tia studied information technology.",
	"Mr Batchelor studied information technology.",
	"Mr Batchelor studied information technology in the Dominican Republic.",
	"Mr Batchelor studied information technology at Boston University",
	"Did Mr Batchelor go to Boston University.",
	"Some woman went to Boston University.",
	"Some man went to Boston University.",
	"Some boy went to Boston University.",
	"Some man raised Mr Batchelor.",
	"A man raised Tia",
	"Some woman told a story.",
	"Did the boss work in the Dominican Republic?",
	"Did the boss work in the company?",
	"The boss worked on the company.",
	"The boss worked at the company.",
	"Did the boss work at a hospital?",
	"The boss worked on a hospital.",
	"The boss worked in a hospital.",
	"The boss worked as a comment",
	"Did the job work at a hospital?",
	"The job worked on a hospital.",
	"The job worked in a hospital.",
	"Did the job disappoint the boss?",
	"Did Mr Batchelor's daughter study information technology?",
	"Mr Batchelor's daughter studied information technology.",
	"The boss studied information technology.",
	"Did the boss study in the United States?",
	"The boss studied information technology at a hospital.",
	"The boss studied information technology at school",
	"Did the boss go to school.",
	"Some woman went to school.",
	"Some man went to school.",
	"Some boy went to school.",
	"Some man raised the boss.",
	"A man raised Mr Batchelor's daughter",
	"Some woman told a story.",
	"Did the boss come from the Dominican Republic?",
	"Did Mr Batchelor immigrate?",
	"Did the boss immigrate to the United States?",
	"Did Mr Batchelor go to the United States?",
	"Did Mr Batchelor come from the Dominican Republic?",
	"Did Mr Batchelor's daughter come to the United States?"
	]

ditransitive_tests = [
	"Mr Batchelor told a story.",
	"Mr Batchelor told Mr Batchelor's father a story.",
	"Mr Batchelor told a story to Mr Batchelor's daughter.",
	"Mr Batchelor told a story to Tia",
	"Mr Batchelor gave some job to Tia.",
	"Did Mr Batchelor give some job to Tia.",
	"Did Mr Batchelor give the job to Tia?",
	"Did Mr Batchelor give the job to someone?",
	"Mr Batchelor gave several job to Tia.",
	"Did someone give something to Tia?",
	"A woman gave the job to Tia.",
	"A woman gave the job to someone.",
	"A woman gave something to someone.",
	"Someone gave something to someone.",
	"Mr Batchelor gave Tia some job.",
	"Did Mr Batchelor give Tia some job?",
	"Did Mr Batchelor give Tia the job?",
	"Did Mr Batchelor give someone the job?",
	"Mr Batchelor gave Tia several job.",
	"Did someone give Tia something?",
	"A man gave Tia the job.",
	"A boy gave Tia the job.",
	"Mr Payne gave Tia the dress.",
	"A man gave someone the job.",
	"A man gave someone something.",
	"Someone gave someone something.",
	"The boss told a story.",
	"The boss told the job a story.",
	"The boss told a story to the job.",
	"The boss told a story to Mr Batchelor's daughter",
	"The boss gave some comments to Mr Batchelor's son.",
	"Did the boss give some comments to Mr Batchelor's son.",
	"Did the boss give the comments to Mr Batchelor's daughter?",
	"Did the boss give the comments to someone?",
	"The boss gave several comments to Mr Batchelor's son.",
	"Did someone give something to Mr Batchelor's daughter?",
	"A woman gave the comments to Mr Batchelor's son.",
	"A woman gave the comments to someone.",
	"A woman gave something to someone.",
	"Someone gave something to someone.",
	"The boss gave Mr Batchelor's daughter some comments",
	"Did the boss give Mr Batchelor's daughter some comments?",
	"Did the boss give Mr Batchelor's daughter the comments?",
	"Did the boss give someone the comments?",
	"The boss gave Mr Batchelor's daughter several comments",
	"Did someone give Mr Batchelor's daughter something?",
	"A man gave Mr Batchelor's daughter the comments",
	"A man gave Mr Batchelor's daughter some comments",
	"A boy gave Mr Batchelor's daughter the comments",
	"The job gave Mr Batchelor's daughter the worker.",
	"A man gave someone the comments",
	"A man gave someone something.",
	"Someone gave someone something.",
	"Did the boss work at a hospital?",
	"The boss did cleaning at a hospital.",
	"Did Mr Batchelor's daughter spell names in the company?"
	]

wh_questions =[
	"Who was the man that interviewed the woman?",
	"What did the woman that the man interviewed study?",
	"Who was a woman that wore a red dress?",
	"Who was the woman that wore a red dress?",
	"Who worked?",
	"Who did Tia teach?",
	"Who taught Tia?",
	"Who gave the job to Tia?",
	"Who gave some job to Tia?",
	"Which person worked?",
	"Which person did Tia teach?",
	"Which subjects did Mr Payne teach?",
	"To whom did Mr Batchelor give some job?",
	"Who did Mr Batchelor give some job to?",
	"Who looked back?",
	"Which man looked back?",
	"Who raised Mr Batchelor's daughter?",
	"Which woman raised Mr Batchelor's daughter?",
	"Who gave the comments to Mr Batchelor's daughter?",
	"Who gave some comments to Mr Batchelor's daughter?",
	"Which person looked back?",
	"Which woman appreciated the boss?",
	"Which girl appreciated the boss?",
	"Which daughter appreciated the boss?",
	"Who did Mr Batchelor's daughter appreciate?",
	"Which person did Mr Batchelor's daughter appreciate?",
	"Which man did Mr Batchelor's daughter appreciate?",
	"Which woman did Mr Batchelor's daughter appreciate?",
	"Which thing did Mr Batchelor's daughter appreciate?",
	"Which worker did Mr Batchelor's daughter appreciate?",
	-- "To whom did the boss give some languages?",
	-- "Who did the boss give some languages to?",
	"Who had a worker?",
	"What did Mr Batchelor's daughter have?",
	"Who did Mr Batchelor's daughter have?",
	"Who did the job disappoint?",
	"Who did the boss's daughter appreciate?",
	"What did the boss's daughter appreciate?",
	"Which thing did the boss's daughter appreciate?",
	"Which man did the boss's daughter appreciate?",
	"Which woman did the boss's daughter appreciate?",
	"Which worker did the boss's daughter appreciate?",
	"What did someone have?"
	]

relclauses = [
	"The woman that a man interviewed studied business law.",
	"The man that interviewed the woman offended the woman.",
	"Mr Batchelor interviewed a woman who wore a red dress.",
	"The man who taught business law was Mr Batchelor.",
	"The man who taught Tia business law was Mr Payne.",
	"The man who taught business law to Tia was Mr Payne.",
	"The man who interviewed the woman was Mr Batchelor.",
	"The woman who wore a red dress was Tia",
	"Was the woman who wore a red dress Tia?",
	"A man who taught Tia worked.",
	"The man who taught Tia worked.",
	"Did the man who taught Tia work?",
	"Did every woman who taught Tia work?",
	"The woman who gave the job to Tia worked.",
	"Mr Batchelor divorced the man that she gave the job to.",
	"Who killed the man that helped the woman " 
	 ++ "that had a boyfriend.",
	"A woman who raised Mr Batchelor's daughter looked back.",
	"The woman who raised Mr Batchelor's daughter looked back.",
	"Did the woman who raised Mr Batchelor's daughter look back?",
	"Did every person who raised Mr Batchelor's daughter look back?",
	"Did some person who raised Mr Batchelor's daughter look back?",
	"The woman who gave the comments to Mr Batchelor looked back.",
	"Mr Batchelor married the man that gave the comments to Mr Batchelor's son.",
	"The man that Mr Batchelor married gave the comments to Mr Batchelor's son.",
	"The man Mr Batchelor married gave the comments to Mr Batchelor's son.",
	"The job disappointed the man that gave Mr Batchelor's daughter the comments",
	"The man that the job disappointed left the Dominican Republic.",
	"The man the job disappointed left the Dominican Republic.",
	"Mr Batchelor's daughter accepted the comments that a man gave Mr Batchelor's son.",
	"Mr Batchelor's daughter accepted the comments that a man gave to Mr Batchelor's son.",
	"Who appreciated the man that gave the girl \
	 \that left the Dominican Republic a comment?"
	]



lf0 = Rel "worked" [ Const(ents!!17) ]
lf00 = (Conj [(Rel "person" [Var 0]), (Rel "worked" [Var 0]) ] ) 
-- lf000 = (Exists (Conj [(Rel "person" [Var 0]), (Rel "worked" [Var 0]) ] )) (Const(ents)!!17)

lf1 = (Equi  (Rel "married" [ Const(ents!!9), Const(ents!!1) ]) (Neg (Rel "married" [ Const(ents!!8), Const(ents!!17)]) ) )

lf2 = (Conj [ (Rel "married" [ Const (ents !! 9), Const       (ents !! 1)]), (Rel "married" [ Const (ents !! 8), Const (ents !!   17)]) ] )

lf3 = Rel "married" [ Const (ents !! 8), Const (ents !! 17)]
lf4 = (Impl  (Rel "married" [ Const (ents !! 9), Const        (ents !! 1)]) (Rel "married" [ Const (ents !! 8), Const (ents !!    17)])  )
lf5 = (Conj [ (Rel "married" [ Const (ents !! 9), Const       (ents !! 1)]), (Rel "married" [ Const (ents !! 8), Const (ents !!   17)]) ] )
lf6 = (Disj [ (Rel "married" [ Const (ents !! 9), Const       (ents !! 1)]), (Rel "married" [ Const (ents !! 8), Const (ents !!   17)]) ] )

lf70 = ( \x -> ( Conj [ (Rel "daughter" [x]), (Rel "have" [x, Const (ents !! 8)]) ] ) ) (Const (ents !! 12) )
lf71 = ( \x -> ( Conj [ (Rel "daughter" [x]), (Rel "have" [x, Const (ents !! 17)]) ] ) ) (Const (ents !! 12) )
lf72 = ( \x -> ( Conj [ (Rel "daughter" [x]), (Rel "have" [x, Const (ents !! 17)]) ] ) ) (Const (ents !! 12) )
lf73 = \x -> Conj [ (Rel "daughter" [x]), (Rel "have" [x, Const (ents !! 17)]) ]
lf74 = ( \x -> ( Conj [ (Rel "daughter" [x]), (Rel "have" [x, Const (ents !! 17)]) ] ) )
lf75 = \x -> Impl (Rel "daughter" [x]) (Rel "have" [x, Const (ents !! 17)])
