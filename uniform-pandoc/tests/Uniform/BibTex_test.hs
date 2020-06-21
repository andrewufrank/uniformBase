---------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE StandaloneDeriving     #-}

module Uniform.BibTex_test  -- (openMain, htf_thisModuelsTests)
     where

import Test.Framework
import Text.BibTeX.Entry
import Uniform.BibTex
import Uniform.Strings

bib1 =  "/home/frank/Workspace8/TestSimple/publications/publicationsPure.bib"
bib1pure =  "/home/frank/Workspace8/TestSimple/publications/publications.bib"
bibaf = "/home/frank/Workspace8/TestSimple/publications/BibTexLatex.bib"

test_read = do
            bib <- readBibTex bib1
            assertEqual resRead (take 40  bib)

test_readaf = do
            bib <- readBibTex bibaf
            assertEqual resReadA (take 40  bib)

resRead =  "@InProceedings{alfer2002beginning,\n  aut"
--resRead =  "% Encoding: UTF-8\n@InProceedings{alfer20"
resReadA = "% Encoding: UTF-8\n\n@InProceedings{navrat"

test_parse = do
    bib <- readBibTex bib1
    entries <- parseBibTex bib
    assertEqual resParse ( take 2  entries)  
test_parseaf = do
    bib <- readBibTex bibaf
    entries <- parseBibTex bib
    assertEqual resParseA ( take 2  entries)

resParse =
 [Cons{entryType = "InProceedings", 
      identifier = "alfer2002beginning",
      fields =
        [("author", "Alfer, Ralf and Thielemann, Henning"),
         ("title", "The beginning of everything"),
         ("booktitle", "42 articles about everything"),
         ("subtype", "reviewed"), ("year", "2002"), ("month", "June")]},
 Cons{entryType = "Article", identifier = "thielemann2000foobar",
      fields =
        [("author", "Thielemann, Henning"), ("title", "Foo Bar"),
         ("journal", "Journal of Irrelevant Applications"),
         ("year", "2000"), ("volume", "88"), ("number", "7"),
         ("pages", "123--321"), ("month", "December"), ("groups", "group1"),
         ("subtype", "popular")]}]

resParseA =
    [Cons{entryType = "InProceedings", identifier = "navratil08",
      fields =
        [("author",
          "Navratil, Gerhard and Karimipour, Farid and Frank, Andrew U."),
         ("title", "Lifting Imprecise Values"),
         ("booktitle", "The European Information Society"),
         ("year", "2008"),
         ("editor",
          "Bernard, Lars and Friis-Christensen, Anders and Pundt, Hardy"),
         ("isbn", "978-3-540-78945-1"), ("pages", "79--94"),
         ("url", "http://publik.tuwien.ac.at/files/pub-geo-2321.pdf"),
         ("abstract", "The article ."),
         ("file",
          "docs/docs4/4283_gnfkaf_Lifting_AGILE_08_pub-geo_2321[1].pdf"),
         ("keywords", "Imprecise values, error propagation, lifting"),
         ("owner", "frank"), ("timestamp", "2018.11.29")]},
 Cons{entryType = "InProceedings", identifier = "Frank2010a",
      fields =
        [("author", "Andrew U. Frank"),
         ("title",
          "What Is the Use of Ontologies Concerning Organizing Data in Multidisciplinary Projects?"),
         ("booktitle", "Mining in European History Conference"),
         ("year", "2010"), ("note", "print approval 12. Mai 2010"),
         ("publisher", "Universit\228t Innsbruck"),
         ("chapter",
          "Session IX: Documentation of Archaeological Excavations and GIS-supported Integrative Data Management"),
         ("pages", "9"), ("abstract", "A central task"),
         ("file", "docs/docs4/4861_Himat_2010.pdf"), ("groups", "authorAF"),
         ("keywords", "DB, Onto"), ("owner", "gruber"),
         ("timestamp", "2010.07.09")]}]

--test_filter = do
--    bib <- readBibTex
--    entries <- parseBibTex bib
--    let entries1 = filterByGroup "group1" entries
--    assertEqual resFilter1 ( take 2 $ entries1)

test_filter2 = do
    bib <- readBibTex bib1
    entries <- parseBibTex bib
    let entries1 = filterByGroup "group1" entries
    assertEqual resFilter1 ( take 2  entries1)

test_filter2af  = do
    bib <- readBibTex bibaf  
    entries <- parseBibTex bib
    let entries1 = filterByGroup "authorAF" entries
    assertEqual resFilter2 ( take 2  entries1)

resFilter2 =
    [Cons{entryType = "InProceedings", identifier = "Frank2010a",
      fields =
        [("author", "Andrew U. Frank"),
         ("title",
          "What Is the Use of Ontologies Concerning Organizing Data in Multidisciplinary Projects?"),
         ("booktitle", "Mining in European History Conference"),
         ("year", "2010"), ("note", "print approval 12. Mai 2010"),
         ("publisher", "Universit\228t Innsbruck"),
         ("chapter",
          "Session IX: Documentation of Archaeological Excavations and GIS-supported Integrative Data Management"),
         ("pages", "9"), ("abstract", "A central task"),
         ("file", "docs/docs4/4861_Himat_2010.pdf"), ("groups", "authorAF"),
         ("keywords", "DB, Onto"), ("owner", "gruber"),
         ("timestamp", "2010.07.09")]}]

resFilter1 =
    [Cons{entryType = "Article", identifier = "thielemann2000foobar",
      fields =
        [("author", "Thielemann, Henning"), ("title", "Foo Bar"),
         ("journal", "Journal of Irrelevant Applications"),
         ("year", "2000"), ("volume", "88"), ("number", "7"),
         ("pages", "123--321"), ("month", "December"), ("groups", "group1"),
         ("subtype", "popular")]}
     , Cons{entryType = "TechReport",
      identifier = "thielemann2000prefoobar",
      fields =
        [("author", "Thielemann, Henning"),
         ("title", "Before Foo becomes Bar"),
         ("institution", "University of Applied Irrelevance"),
         ("year", "2000"), ("groups", "group1")]
         }]

test_filerPure = assertEqual resFilter  (filterByGroup "group1" resParse)

resFilter =
    [Cons{entryType = "Article", identifier = "thielemann2000foobar",
      fields =
        [("author", "Thielemann, Henning"), ("title", "Foo Bar"),
         ("journal", "Journal of Irrelevant Applications"),
         ("year", "2000"), ("volume", "88"), ("number", "7"),
         ("pages", "123--321"), ("month", "December"), ("groups", "group1"),
         ("subtype", "popular")]}]


test_getIds2 = do
    bib <- readBibTex bibaf
    entries <- parseBibTex bib
    let entries1 = filterByGroup "authorAF" entries
    let ids = getBibIdentifier entries1
    assertEqual ["Frank2010a"] ids
