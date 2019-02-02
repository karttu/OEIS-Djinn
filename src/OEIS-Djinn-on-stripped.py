#!/usr/bin/python

#
# oeis-djinn-on-stripped (the previous version of this program was known with name eq_class_match_test.py)
#                        -- Simple program to test equivalence class matching against OEIS
#                           data in "stripped" -file, written by Antti Karttunen
#                           (firstname.surname@gmail.com) Nov 10 - Nov 11 2016, based on
#                           his earlier Python-scripts (permchek.py, oeischek.py)
#
# Please feel free to develop these ideas (or this script) in the spirit of OEIS Contributor's License:
#   https://oeis.org/wiki/The_OEIS_Contributor%27s_License_Agreement
#
# Should run also in cloud.sagemath.com
#
# Last edited 2017-08-25 by Antti Karttunen

# 2017-05-19   Added the check for the minimum size of the second largest eq.class of the matching sequence,
#              which allows us to prune off many false positives that are "almost injective", e.g.,
#              sequences that obtain unique values in the midst of "background sea" of 0's or 1's.
#

# 2017-08-10   Use new function get_nonsingleton_inverses instead of get_inverses
#              This should leave off a large amount of "true positive, but irrelevant" matches
#              for a certain kind of filtering sequences like A014682.
#

# 2017-08-24   Implemented setwise difference of matched sets (changed the main looping order also).
#              Now count_almost_injections_from_stripped_data also filters off monotonic non-injections.
#

#
# 2018-10-09   Added a rudimentary main routine for sanity's sake. Still needs some on-the-fly editing
#              for running with different parameters and inputs.
#
#
# 2018-11-08   Corrected a nasty bug in read_bfile_as_a_list which botched the reading of b-files with negative terms.
#

import os
import re
import sys

org_datafile = "./stripped"
datafile = "./non-injections.txt"
namefile = "./names"



at_least_n_distinct_classes = 2
max_diff_for_two_largest_classes = 150 # 
minlength_for_seq2 = 30
minsize_for_the_second_largest_eq_class = 2

singleton_classes_also = True # Also matches with only singleton equivalence classes are considered


def main():
  if(len(sys.argv) > 1):
    if("--help" == sys.argv[1]):
      sys.stdout.write("Usage: " + sys.argv[0] + "\n   For ordinary usage involving stripped and names downloaded from https://oeis.org/wiki/Welcome#Compressed_Versions\n\n")
      sys.stdout.write("Usage: " + sys.argv[0] + " bfile1 bfile2 ... bfilen\n    Given n text-files in b-file format, output a report whether each distinct pair A, B seems to satisfy A => B or not.\n")
      sys.stdout.write("\nFor example, by giving file http://oeis.org/A101296/b101296.txt as one of the arguments, it can be immediately seen if the sequences for other b-files certainly are not dependent on prime signature only.\n")
      exit(1)

    off = 0
    filenames = sys.argv[1:]
    seqs = []
    inverses = []
    for filename in filenames:
      seq = read_bfile_as_a_list(filename)
      seqs.append(seq)
      inverses.append(get_inverses(seq,off))

    for i in range(len(seqs)):
      for j in range(len(seqs)):
        sys.stdout.write("\nIt seems that " + filenames[i] + " (" + str(len(seqs[i])) + " terms) => " + filenames[j]  + " (" + str(len(seqs[j])) + " terms) is: ")
        result = has_coarser_eq_classes_with_threshold(seqs[j],off,inverses[i],at_least_n_distinct_classes)
        if(result): result = (result >= at_least_n_distinct_classes)
        print result
      print "---"
      print ""
  else:
    if((not os.path.exists(datafile)) or (os.path.exists(org_datafile) and (os.stat(datafile).st_mtime < os.stat(org_datafile).st_mtime))):
      if(not os.path.exists(datafile)): sys.stdout.write("File " + datafile + " does not exist, preparing it from " + org_datafile + "\n")
      else: sys.stdout.write("File " + datafile + " exists, but is out of date, repreparing from " + org_datafile + "\n")
      if(not os.path.exists(org_datafile)):
        sys.stdout.write("File " + org_datafile + " does not exist either, please download it with command:\n  wget https://oeis.org/stripped.gz\nand then\n  gunzip stripped.gz\nDo the same with https://oeis.org/names.gz\n")
        exit(1)

      
      count_almost_injections_from_stripped_data(org_datafile,datafile,8)

    main_loop_over_datafile(selected_anums,datafile,namefile,outputfile,at_least_n_distinct_classes,max_diff_for_two_largest_classes,minlength_for_seq2,minsize_for_the_second_largest_eq_class)


outputfile = "./eqout_for_A322974_with_singletons_and_slec2_2019-01-15.txt"

selected_anums = [
"A322974",
#
# "A007814", # (576)
# "A010878", # (223) mod 9
# "A010881", # (628) mod 12
# "A323370", # (2350)  "./eqout_for_A323370_sans_periodics_with_singletons_and_slec2_2019-01-15.txt"
#
# "A323374", # (306)  "./eqout_for_A323374_with_singletons_and_slec2_2019-01-15.txt"
#
# "A323370", # (2951)
# "A323405", # (637, 3588) "./eqout_for_A323405_sans_A323370_with_singletons_and_slec2_2019-01-15.txt"
# 
# "A291751", # (838)
# "A323372", # (1647, 2484) "./eqout_for_A323372_sans_A291751_with_singletons_and_slec2_2019-01-15.txt"
#
# "A007814", # (576)
# "A010878", # (223) mod 9
# "A010881", # (628) mod 12
# "A305801"  # (4254, 866 non-periodics, etc. without b-file) "./eqout_for_A305801_sans_periodics_with_singletons_and_slec2_2019-01-12.txt"
#
# "A007814", # (576)
# "A323236" # (74, 612) "./eqout_for_A323236_sans_A007814_with_singletons_and_slec2_2019-01-12.txt"
#
# "A007814", # (576)
# "A323235" # (39, 484) "./eqout_for_A323235_sans_A007814_with_singletons_and_slec2_2019-01-12.txt"
#
# "A007814", # (576)
# "A323241" # (90, 666)  "./eqout_for_A323241_sans_A007814_with_singletons_and_slec2_2019-01-12.txt"
#
# "A007814", # (576)
# "A323242" # (39, 302)  "./eqout_for_A323242_sans_A007814_with_singletons_and_slec2_2019-01-12.txt"
#
# "A323241" # (666)
#
#
#
# "A323238" # (883) "./eqout_for_A323238_with_singletons_and_slec2_2019-01-12.txt"
#
# "A323237" # (934)
#
# "A323242" # (302)
#
# "A323241" # (666)
#
# "A323236", # (612)
#
# "A323235" # (484) "./eqout_for_A323235_with_singletons_and_slec2_2019-01-12.txt"
#
# "A323234" # (1420) "./eqout_for_A323234_with_singletons_and_slec2_2019-01-12.txt"
#
# "A323156" # (176) "./eqout_for_A323156_with_singletons_and_slec2_2019-01-12.txt"
#
# "A323240", # (288) "./eqout_for_A323240_with_singletons_and_slec2_2019-01-12.txt"
#
# "A323168" # (194) "./eqout_for_A323168_with_singletons_and_slec2_2019-01-12.txt"
#
# "A323075", # "./eqout_for_A323075_with_singletons_and_slec2_2019-01-05.txt"
#
# "A322826" # "./eqout_for_A322826_with_singletons_and_slec2_2019-01-05.txt"
#
# "A007814",
# "A319701"  # (1179) "./eqout_for_A319701_sans_A007814_with_singletons_and_slec2_2019-01-05.txt"
#
# "A323080", # (54) "./eqout_for_A323080_with_singletons_and_slec2_2019-01-05.txt"
#
# "A323078" # (31) "./eqout_for_A323078_with_singletons_and_slec2_2019-01-05.txt"
#
# "A323082" # (191) "./eqout_for_A323082_with_singletons_and_slec2_2019-01-05.txt"
#
# "A323079", # (193) "./eqout_for_A323079_with_singletons_and_slec2_2019-01-05.txt"
#
# "A322988" # "./eqout_for_A322988_with_singletons_and_slec2_2019-01-03.txt"
#
# "A322824", # (131) # "./eqout_for_A322824_with_singletons_and_slec2_2018-12-30.txt"
#
# "A322822", # (41 ) "./eqout_for_A322822_with_singletons_and_slec2_2018-12-30.txt"
#
# "A322816", # (21) "./eqout_for_A322816_with_singletons_and_slec2_2018-12-30.txt"
#
# "A322807", # (67) "./eqout_for_A322807_with_singletons_and_slec2_2018-12-30.txt"
#
# "A001221", # (28)
# "A322814", # (31, 59) "./eqout_for_A322814_sans_A001221_with_singletons_and_slec2_2018-12-27.txt"
#
# "A322805", # (33)  "./eqout_for_A322805_with_singletons_and_slec2_2018-12-27.txt"
#
# "A010877", # (730)
# "A322588", # (652, 698) "./eqout_for_A322588_sans_mod8_with_singletons_and_slec2_2018-12-19.txt"
#
# "A000035", # (263)
# "A322591" # (431) "./eqout_for_A322591_sans_others_with_singletons_and_slec2_2018-12-19.txt"
#
# "A000035", # (263)
# "A101296", # (828)
# "A322314", # (184, 1275)  "./eqout_for_A322314_sans_others_with_singletons_and_slec2_2018-12-19.txt"
#
# "A322587", # (385) "./eqout_for_A322587_with_singletons_and_slec2_2018-12-19.txt"
#
# "A322588", # (698) "./eqout_for_A322588_with_singletons_and_slec2_2018-12-19.txt"
#
# "A322589" #  (34) "./eqout_for_A322589_with_singletons_and_slec2_2018-12-19.txt"
#
# "A322592" # (29) "./eqout_for_A322592_with_singletons_and_slec2_2018-12-19.txt"
#
# "A322591" # (431) "./eqout_for_A322591_with_singletons_and_slec2_2018-12-19.txt"
#
# "A101296", # (817)
# "A322021" # (734, 1551 in total) "./eqout_for_A322021_sans_A101296_with_singletons_and_slec2_2018-12-04.txt"
#
# "A322024", # 
#
# "A322023", # (8)
#
# "A305891", # (1693)
# "A322022" # (727, 2420 in total)
#
# "A007814", # (574)
# "A322315", # (209, 783 in total) #  "./eqout_for_A322315_sans_A007814_with_singletons_and_slec2_2018-12-04.txt"
#
# "A322311" # (24) "./eqout_for_A322311_with_singletons_and_slec2_2018-12-04.txt"
#
# "A320110", #  "./eqout_for_A320110_with_singletons_and_slec2_2018-11-23.txt"
#
# "A007814",
# "A319349"   # "./eqout_for_A319349_sans_A007814_with_singletons_and_slec2_2018-11-09.txt"
#
# "A319999",
#
# "A320018" # "./eqout_for_A320018_with_singletons_and_slec2_2018-11-09.txt"
#
# "A101296", # (813)
# "A305896", # (176) "./eqout_for_A305896_sans_A101296_with_singletons_and_slec2_2018-11-08.txt"
#
# "A010877", # (729)
# "A319997", # (68, 505)  "./eqout_for_A319997_sans_A010877_with_singletons_and_slec2_2018-11-01.txt"
#
# "A010877", # (729)
# "A319698" # (619 , 1086)  "./eqout_for_A319698_sans_A010877_with_singletons_and_slec2_2018-11-01.txt"
#
# "A101296", # (812)
# "A318887" # (415) # "./eqout_for_A318887_sans_A101296_with_singletons_and_slec2_2018-10-24.txt"
#
# "A010877", # (729)
# "A319682", # (261, 309) #  "./eqout_for_A319682_sans_A010877_with_singletons_and_slec2_2018-10-24.txt"
#
# "A007814", # (573)
# "A097272" # (1115)
#
# "A007814", # (573)
# "A319701", # (1155, tot 1728)
#
# "A305801" # (4668) "./eqout_for_A305801_with_singletons_and_slec2_2018-10-09.txt"
#
# "A101296",
#
# "A319709"  # (457) "./eqout_for_A319709_with_singletons_and_slec2_2018-09-30.txt"
#
# "A319996" # (2804)  "./eqout_for_A319996_with_singletons_and_slec2_2018-10-09.txt"
# "A319707" # (5887)  "./eqout_for_A319707_with_singletons_and_slec2_2018-09-30.txt"
#
#
# "A003557", # (233)
# "A051953", # (297)
# "A007814", # (255)
# "A319348", # (699) # "./eqout_for_A319348_sans_others_with_singletons_and_slec2_2018-09-30.txt"
#
# "A101296", # (803)
# "A295300"  # (2663, 3466 in total)
#
# "A101296", # (803)
# "A300247"  # (465)  "./eqout_for_A300247_sans_others_with_singletons_and_slec2_2018-09-29.txt"
#
# "A010877", # (729)
# "A003415", # (66)
# "A300252", # (15)
# "A300245", # (698)  "./eqout_for_A300245-sans-others-2018-09-29.txt"
#
# "A003557", # (233)
# "A051953", # (297)
# "A007814", # (255)
# "A319349", # (1022, 1805 in total) # "./eqout_for_A319349_sans_others_with_singletons_and_slec2_2018-09-29.txt"
#
# "A007814", # (573)
# "A319338"  # (35)
#
# "A010877", # (729)
# "A305800" # (with slec2 = 3228, total = 3957) "./eqout_for_A305800_sans_A010877_with_singletons_and_slec2_2018-09-27.txt"
#
# "A003557", # (233)
# "A007814", # (507)
# "A101296", # (709)
# "A319347", # (362)
#
# "A305800" # (with slec2 = 3669)
#
# "A319706", # (3742, with slec1 = 4097) # for primes p records the prime signature of 2p+1
#
# "A319705", # (5988) # for primes p records A286622(p)
#
# "A319704", # (4924) # primes mod 4
#
# "A305801" # (4629 with singletons but slec=2)
#
# "A319351", # (2735) Cf. "A305975" --> 1516)
#
# "A319350", # (154? 5362 with singletons also, 4808 with singletons but slec=2)
#
# "A003557", # (234)
# "A007814", # (507)
# "A101296", # (709)
# "A319347", # (303, 1839 in total)  "./eqout_for_A319347_without_singletons_2018-09-25.txt" (OR this: Check missing b-files)
#
# "A007814", # (574)
# "A001065", # (48)
# "A319346", # (217)  "./eqout_for_A319346_without_singletons_2018-09-25.txt"
#
# "A003557", # (234)
# "A007814", # (507)
# "A101296", # (710)
# "A319347", # (388, 1839 in total)  "./eqout_for_A319347_sans_others_2018-09-25.txt" Check missing b-files for THIS!
#
# "A007814", # (574)
# "A001065", # (96)
# "A319346", # (1867, 2529 in total)  "./eqout_for_A319346_sans_others_2018-09-25.txt"
#
# "A009191", # (5)
# "A101296", # (803)
# "A319337", # (46, 854 in total)  "./eqout_for_A319337_sans_others_2018-09-25.txt"
#
# "A000005", # (109)
# "A319355", # (23, 132 in total)  "./eqout_for_A319355_sans_others_2018-09-25.txt"
#
# "A000005", # (109)
# "A000035", # (263)
# "A319353", # (110, 471 in total)  "./eqout_for_A319353_sans_others_2018-09-25.txt"
#
# "A009191", # (5)
# "A009194", # (5)
# "A286591", # (3, thus 13 in total)
#
# "A000005", # (109)
# "A000035", # (263)
# "A319343", # (192)
# "A319345",  # (1088, 1543) "./eqout_for_A319345_sans_others_2018-09-23.txt"
#
# "A000005", # (109)
# "A319343"  # (444)
#
# "A000010", # (104)
# "A101296", # (802)
# "A318893", # (1789)
#
# "A061395", # (55)
# "A101296", # (802)
# "A318891", # (495) # "./eqout_for_A318891_sans_others_2018-09-23.txt"
#
# "A007814", # (574)
# "A101296", # (801)
# "A305891", # (317) "./eqout_for_A305891_sans_others_2018-09-23.txt"
#
# "A305801", # (5161)  "./eqout_for_A305801_2018-09-23.txt"
#
# "A101296", # (794)  "./eqout_for_A101296_2018-09-06.txt"
#
# "A001222",
# "A318470", # "./eqout_for_A318470_sans_bigomega_2018-09-06.txt"
#
# "A001222",
# "A318469" # Is A256693 From fifth root of Riemann zeta function: form Dirichlet series Sum b(n)/n^x whose fifth power is zeta function; sequence gives denominator of b(n). REAL?
#
#
# "A318831" # Only A295660 Binary weight of Euler phi: a(n) = A000120(A000010(n)) found.
#
# "A318832" # Only A175548 Binary weight of sigma(n) found.
#
# "A051953",
# "A318835", # Only 40 matches in non-cototient portion.
#
# "A318837",  "./eqout_for_A318837_2018-09-06.txt" 12 matches, interesting1
#
# "A000010",
# "A318839", "./eqout_for_A318839_sans_phi_2018-09-06.txt" Some interesting non-phi matches!
#
# "A318308", "./eqout_for_A318308_2018-09-02.txt"
#  
# "A318308",
# "A000120",
# "A286449",
# "A318310", "./eqout_for_A318310_sans_others-2018-09-02.txt"
#  
# "A101296", # "./eqout_for_A101296-2018-07-03.txt"
# "A305975", # (686) "./eqout_for_A305975-sans-A101296-also-with-singletons-2018-08-06.txt"
#
# "A305896", # (946) "./eqout_for_A305896-2018-07-03.txt"
#
# "A305898", # (750) "./eqout_for_A305898-2018-07-03.txt"
#
# "A305800", # (3825) "./eqout_for_A305800-2018-07-03.txt"
#
# "A305980", # (545) "./eqout_for_A305980-2018-07-03.txt"
#
# "A305979", # (687)  "./eqout_for_A305979-2018-07-03.txt"
#
# "A305975", # (1407) "./eqout_for_A305975-2018-07-03.txt"
#
# "A305894", # (5407) "./eqout_for_A305894-2018-07-03.txt"
#
# "A290110", "./eqout_for_A290110-2018-06-11.txt"
#
# "A001065",
# "A101296",
# "A300235", "./eqout_for_A300235_sans_others-2018-06-11.txt"
#
# "A101296",
# "A305789", "./eqout_for_A305789_sans_A101296-2018-06-11.txt"
#
# "A101296",
# "A305790", "./eqout_for_A305790_sans_A101296-2018-06-11.txt"
#
# "A302046",
#
# "A032742", "./eqout_for_A032742-2018-03-25.txt"
#
# "A051953",
# "A300825", "./eqout_for_A300825-sans-cototient-2018-03-25.txt"
#
# "A001065",
# "A300835",
#
# "A010877",
# "A003415",
# "A300252",
# "A300245", "./eqout_for_A300245-sans-others-2018-03-25.txt"
#
# "A000035",
# "A297169", # outputfile = "./eqout_for_A297169-sans-A000035-2018-03-06.txt"
#
# "A101296",
# "A300224", "./eqout_for_A300224-sans-A101296-2018-03-06.txt"
#
# "A300225",
#
# "A000035",
# "A300242", "./eqout_for_A300242-sans-A000035-2018-03-06.txt"
#
# "A101296",
# "A300248",
#
# "A001615",
# "A101296",
# "A295888", "./eqout_for_A295888-sans-others-2018-02-12.txt"
#
# "A065769",
#
# "A297113",
#
# "A286376", "./eqout_for_A286376_with_singletons_also.txt"
#
# "A001221",
# "A295666",
# "A296203", # "./eqout_for_A296203_sans_others.txt"
#
# "A000035",
# "A051953",
#
# "A000035",
# "A239968" "./eqout_for_A239968_sans_A000035_and_with_singletons_included-2017-12-16.txt"
#
# "A293215",
# "A293217",
#
# "A001065",
# "A293226", "./eqout_for_A293226_sans_A001065-2017-12-06.txt"
#
# "A000010",
# "A003557",
# "A101296",
# "A295886", "./eqout_for_A295886_sans_others-2017-12-06.txt"
#
# "A291761",
# "A010877",
# "A286161",
#
# "A000035",
# "A290094",  "./eqout_for_A290094_sans_A000035-2017-12-06.txt"
#
# "A003557",
# "A295886", "./eqout_for_A295886_sans_A003557-2017-12-06.txt"
#
# "A001065",
# "A295885", "./eqout_for_A295885_sans_A001065-2017-12-06.txt"
#
# "A101296",
# "A286603",
# "A295880", "./eqout_for_A295880_sans_others-2017-12-06.txt"
#
# "A296080",
# "A295888",
# "A032742",
#
# "A289626",
#
# "A294897",
#
# "A295666",
# "A294895" "./eqout_for_A294895_sans_A295666.txt"
#
# "A048250",
# "A101296",
# "A291758", "./eqout_for_A291758_sans_others-2017-11-23.txt"
#
# "A101296",
# "A286034", "./eqout_for_A286034_sans_A101296.txt"
#
# "A000035",
# "A051953",
#
# "A286359",
#
# "A295300"
#
# "A294933",
#
# "A294875"
#
# "A294897",
#
# "A294876",
# "A010877",
# "A291751",
#
# "A048250",
#
# "A010877",
# "A003557", # "./eqout_for_A003557_sans_A010877.txt"
#
# "A001222",
# "A293442", "./eqout_for_A293442_sans_A001222.txt"
# "A293226"
# "A293223",
#
# "A010872", # "./eqout_for_A293450_sans_others.txt" # XXX - Note: A293895 should occur as a match, if not
# "A293226",                                         # then something is not right!
# "A293450",                                         # Use get_inverses instead of get_nonsingleton_inverses
#
# "A293232",
#
# "A293215",
#
# "A002326",
# "A292266",
#
# "A060937",
#
# "A292582",
#
# "A083399"
#
# "A292259",
#
# "A292584",
#
# "A286603",
# "A291751",
#
# "A001615",
#
# "A001511",
# "A101296",
# "A291761",
# "A286161",
#
# "A003557",
#
# "A000035",
# "A001511",
#
# "A000035",
# "A101296",
# "A291761",
#
# "A292240",
#
# "A286587",
#
# "A253889",
#
# "A286586",
#
# "A048250",
# "A101296",
# "A291758",
#
# "A003557",
# "A101296",
# "A291757",
#
# "A003557",
# "A000010",
# "A291756",
#
# "A001065",
# "A101296",
# "A291765",
#
# "A291764",
# "A001511",
# "A101296",
# "A286161",
# "A286603"
# "A268819",
# "A101296",
# "A286467"
# "A291784"
# "A101296",
# "A286449",
# "A286592",
# "A286360",
# "A291752"
# "A286603",
# "A291751",
# "A049820"
# "A010877",
# "A014682"
# "A101296",
# "A286480",
# "A101296",
# "A286257",
# "A289626",
# "A000010", # 82
# "A101296", # 622
# "A286160", # 1708 --> 1156 (???) outputfile = "./eqout_for_A286160_sans_A000010_and_A101296.txt"
# "A010877",
# "A014682"
# "A060681",
# "A049820",
# "A289626",
# "A010877",
# "A290094",
# "A290095",
# "A290097"
# "A014682"
# "A032742"
# "A290099"
# "A289626"
# "A101296"

]

selected_anums_some2 = [
"A290094", # Ternary filter, good!
"A289626",
"A289628",
"A290076",
"A290082",
"A290083",
]

selected_anums_some = [
"A046523",
"A289621",
"A289622",
"A289623",
"A289626"
"A286600"
]

selected_anums_for_A032742_related = [
"A032742",
"A286386",
"A286473",
"A286474",
"A286475",
"A286476",
]

selected_anums_for_A286456 = [
"A056239",
"A243503",
"A286456",
]

selected_anums_A286457 = [
"A078898",
"A246277",
"A286457",
]

selected_anums_futil = [
"A286458", # 22745 suspected matching
"A286459"  # 25516 suspected matching (not enough duplicate-terms among the Data-section, thus almost injective!)
]

selected_anums_ordinals = [
"A263017",
"A286478",
"A286552",
"A286554",
"A286558"
]

selected_anums_hofstadter = [
"A004001",
"A005185",
"A286541",
"A286559",
"A286560"
]

# "A286360",
# "A046523"
# "A285334",
# "A286357",

selected_anums_b = [
"A103391",
"A286370",
"A286371",
"A286372",
"A286373",
"A286374",
"A286375",
"A286376",
"A286379",
"A286473",
"A286474",
"A286475",
"A286476"

# "A001511",
# "A046523",
# "A161942",
# "A286034",
# "A286260",

# "A286367",
# "A257993",
# "A278226",
# "A286253",
# "A286254",
# "A286382", # A257993 & A278226 # 15 + 3 = 302 !
# "A286383",
# "A286384",

# "A278221",

# "A046523",
# "A286255",
# "A286256",
# "A286257",
# "A286258",

# "A286243",
# "A286250",
# "A286366",

# "A285334"
# "A278221"
# "A026741"
# "A060681",
# "A161942"
# "A269174", # Rul 124, Many many matches, not all spurious? Use b-files?
]

selected_anums1 = [

"A252463",

"A285728",

"A253554",

"A286104",
"A286107",

"A285722",
"A285723",

"A285732",
"A285733",

"A286153",
"A286155",

"A286101",
"A286102",
"A285724",

"A286156",
"A286157",
"A286158",
"A286159",

"A286160",

"A286161",

# "A252463",
# "A278222",
# "A278531",
# "A278533",
# "A278535"

# "A283990",
# "A283995",

# "A284010",
# "A284011",
# "A284012"
]

selected_anums2 = [ # Various row & column indices, for sieves, etc.
"A000265",
"A001175", # Pisano periods (or Pisano numbers): period of Fibonacci numbers mod n
"A001179", # Leonardo logarithm of n. 

"A065371",
"A063918",
"A001223", # 1+29 matches, all relevant, also A135732?
"A003417", # About 200 matches, many relevant.
"A007913",
"A007949",
"A046523",
"A091223",
"A112591", # 1+3 matches, all relevant, also A237881 "a(n) = 2-adic valuation of prime(n)+prime(n+1)" ???
# Case ...01 and ...11 is possible (twin primes), leaves 2, OK.
# Case ...011 and ...111 also possible (e.g. 19 and 23), leaves 4, (A007814 (+ 19 23)) = 1. 
# Case ?
"A055396",
"A054494",
"A078898",
"A105800",
"A111701", # Noisy?
"A056671",
"A065936",
"A065937",
"A048675",
"A097246",
"A276075",
"A276085",
"A052126",
"A056239",
"A278510",
"A278520",
"A278521",
"A278522",
"A278530"
"A064989",
"A066136",
"A098189",
"A252463",
"A256989",
"A256990",
"A256992",
"A260437",
"A260438",
"A276146",
"A278528",
"A278529",
"A260738",
"A260739",
"A002326",
"A003108",
"A261226",
"A003415",
"A004001",
"A006370",
"A010060",
"A014682",
"A032742",
"A033461",
"A033985",
"A049820",
"A060681",
"A084531",
"A087808",
"A127301",
"A127302",
"A239002",
"A239003",
"A255131",
"A264977",
"A275728",
"A276528",
"A277198",
"A277330",
"A277333", # (Huge terms!) Left inverse of A260443, giving 0 as a result when n is outside of the range of A260443.
"A277886",
"A277895",
"A277903",
"A277906",
"A278158",
"A278160",
"A278162",
"A278165",
"A278217",
"A278218",
"A278219",
"A278220",
"A278221",
"A278222",
"A278223",
"A278224",
"A278225",
"A278226",
"A278227",
"A278228",
"A278229",
"A278230",
"A278231",
"A278232",
"A278233",
"A278234",
"A278235",
"A278236",
"A278237",
"A278240",
"A278241",
"A278242",
"A278243",
"A278244",
"A278245",
"A278246",
"A278247",
"A278248",
"A278249",
"A278250",
"A278251",
"A278252",
"A278253",
"A278254",
"A278255",
"A278256",
"A278257",
"A278258",
"A278259",
"A278260",
"A278261",
"A278262",
"A278263",
"A278264",
"A278266",
"A278488",
"A278497",
"A278501",
"A278502",
"A278523",
"A278524",
"A278525",
"A278526",
"A278527",
"A278531",
"A278532",
"A278533",
"A278534",
"A278535",
"A278540",
"A278541",
"A278542",
"A278543",
"A279350",
"A279354",
###
"A280488",
"A280489",
"A280490",
"A280491",
"A280492",
"A280493",
"A280494",
"A280495",
"A280496",
"A280497",
"A280498",
"A280499",
"A280500",
"A280501",
"A280502",
"A280503",
"A280504",
"A280505",
"A280506",
"A280507",
"A280508",
"A280509",
"A054494", # Largest Fibonacci factor of n.
"A054495", # Smallest k such that n/k is a Fibonacci number.
"A105800", # Greatest Fibonacci number that is a proper divisor of the n-th Fibonacci number
"A280686", # Largest Fibonacci proper divisor of n.
"A280687",
"A280688",
"A280689",
# "A280690", # a(n) = A000045(n) / A105800(n); the n-th Fibonacci number divided by its largest Fibonacci proper divisor.
"A280691",
"A280694",
"A280695",
"A280696",
"A280697",
"A280698",
"A280699",
"A047994",
"A060654",
"A060766",
"A116512",
"A276836",
"A105222",
"A087267",
"A203814",
"A218799",
"A075444",
"A216282",
"A072594",
"A002034",
"A214967",
"A217667",
"A079868",
"A088444",
"A088445",
"A020653",
"A020652",
"A094522",
"A104714",
"A270313",
"A194742",
"A003324",
"A110630",
"A116529",
"A120250",
"A120249",
"A120251",
"A076511",
"A096776",
"A280684"

###
]


def sort_eq_classes_by_size(a,b):
  '''Sorts two tuples a and b by the length of their second element, which should be a list, longest first'''
  (a_val,a_indices) = a
  (b_val,b_indices) = b
  if(len(a_indices) > len(b_indices)): return(-1)
  elif(len(a_indices) < len(b_indices)): return(+1)
  else:
    if(a_val < b_val): return(-1)
    elif(a_val > b_val): return(+1)
    else: return(0)


def sortseqsV1(a,b):
  '''Sorts two sequence-info tuples a and b into appropriate order'''
  (seqA_anum,seqA_name,Acase,seqA_num_of_eq_classes,seqA_sizediff_of_two_largest,seqA_size_of_second_largest_eq_class) = a
  (seqB_anum,seqB_name,Bcase,seqB_num_of_eq_classes,seqB_sizediff_of_two_largest,seqB_size_of_second_largest_eq_class) = b

  if(Acase < Bcase): return(-1)    # Acase and Bcase either "<=>" or "=>". We want the former before the latter.
  elif(Acase > Bcase): return(+1)
  else:
    if(seqA_num_of_eq_classes > seqB_num_of_eq_classes): return(-1)
    elif(seqA_num_of_eq_classes < seqB_num_of_eq_classes): return(+1)
    else:
      if(seqA_sizediff_of_two_largest < seqB_sizediff_of_two_largest): return(-1)
      elif(seqA_sizediff_of_two_largest > seqB_sizediff_of_two_largest): return(+1)
      else:
        if(seqA_anum < seqB_anum): return(-1)
        if(seqA_anum > seqB_anum): return(+1)
        else: return(0)


#
# For example:
# get_inverses(A046523,1)
# {32: [32], 1: [1], 2: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89], 4: [4, 9, 25, 49], 6: [6, 10, 14, 15, 21, 22, 26, 33, 34, 35, 38, 39, 46, 51, 55, 57, 58, 62, 65, 69, 74, 77, 82, 85, 86, 87], 48: [48, 80], 8: [8, 27], 64: [64], 12: [12, 18, 20, 28, 44, 45, 50, 52, 63, 68, 75, 76], 16: [16, 81], 72: [72], 24: [24, 40, 54, 56, 88], 36: [36], 60: [60, 84], 30: [30, 42, 66, 70, 78]}
#
#

def get_inverses(seq,off):
  '''Returns a hash table (dictionary) of all (v, list of positions i where seq(i) = v) for the seq.'''
  seqlen = len(seq)

  inverses = {}

  for i in range(seqlen):
    if seq[i] in inverses: inverses[seq[i]].append(i+off)
    else: inverses[seq[i]] = [i+off]

  return(inverses)



def remove_singletons(inverses):
  '''Return a new dictionary where all singleton points (one-element lists) have been removed from the inverses dictionary'''

  new_inverses = {}

  for key in inverses:
      if(len(inverses[key]) > 1): new_inverses[key] = inverses[key]

  return(new_inverses)

def get_nonsingleton_inverses(seq,off):
  '''Returns a hash table (dictionary) of all (v, list of positions i (at least two disctint) where seq(i) = v) for the seq.'''
  if(singleton_classes_also): return(get_inverses(seq,off))
  else: return(remove_singletons(get_inverses(seq,off)))




# Call:
#
# has_coarser_eq_classes_with_threshold(seq1,off,get_nonsingleton_inverses(seq2,off),at_least_n_distinct_classes)
# returns True iff, based on available data, it holds that:
#
#   for all applicable i, j: seq2(i) = seq2(j) => seq1(i) = seq1(j),
#
# i.e., in terms of https://en.wikipedia.org/wiki/Equivalence_relation#Comparing_equivalence_relations
# the equivalence classes of seq1 are coarser (or equally coarse) than the equivalence classes of seq2,
# and furthermore:
#
#   'seq1' is divided to 'at_least_n_distinct_classes' different equivalence classes.
#
# This last argument (if optionally set to value > 1) provides a way of filtering off
# all constant sequences (A000004, A000012) or near-constant like A063524 or A057427,
# as, although they in the most cases would be real matches (not false positives in technical sense),
# they are still of very little interest for the user.
#
# Neither we want spurious false-positive matches with sequences like these two:
# A103847 McCarthy's 91 Function: a(n) = n-10 if n>100, otherwise a(n) = a(a(n+11)).
# A107844 Highest value obtained in the recursion of McCarthy'a 91 function until it terminates.
# where the first increase from a repeating term does not happen until after n=100.


def has_coarser_eq_classes_with_threshold(seq1,off,inverses_of_seq2,at_least_n_distinct_classes):
  '''Returns non-false if seq1 obtains an identical value for all members of any particular eq.class of seq2, for all of its equivalence classes, given as a dictionary as the third argument. Also seq1 must have at least at_least_n_distinct_classes.'''
  seq1len = len(seq1)

  first_inverses_in_seq1 = {}
  distinct_classes = 0


  for val in inverses_of_seq2:
    e = None
    for k in inverses_of_seq2[val]:
      if k-off >= seq1len:
        continue
      elif (None == e): # e not initialized yet.
# so e is set to the value that seq1 should obtain for ALL k in this particular equivalence class of seq2:
        e = seq1[k-off]
        if not(e in first_inverses_in_seq1): # This e not encountered before?
          first_inverses_in_seq1[e] = 1
          distinct_classes = distinct_classes + 1

      elif seq1[k-off] <> e: # found that seq1 obtains a different value at some point in the same e.c. of seq2,
        return(False)        # thus their equivalence classes do not match, and return false immediately.


  return(distinct_classes)

# If everything went well above, then as a post-check we could check that seq1 (applicable initial portion)
# is not too constant for our taste:
# return(distinct_classes >= at_least_n_distinct_classes)

# Could have done it this way also, however, the above code filters those pseudo-constant seqs like A103847 better:
# return(len(get_inverses(seq1,off).items()) >= at_least_n_distinct_classes)



def has_multiple_inverses_larger_than_threshold_point(inverses,threshold):
    '''Returns true if the dict inverses contains at least one list longer than one with at least one element >= threshold.'''
    for key in inverses:
      invs = inverses[key]
      if(len(invs) > 1):
        for i in invs:
          if(i >= threshold):
            return(True)

    return(False)

def read_namefile_lines_until_anum_at_least_large_is_found(infp2,anum):
    '''Reads infp2 up to the point where the first item >= anum, and then return a tuple (that anum,the rest of that line) (i.e., the name of seq).'''

    nameline = "unknown"
    namelinepat = re.compile(r'^([^ ]+) ')

    while("" != nameline):
      nameline = infp2.readline()
      m2 = namelinepat.match(nameline)
      if(m2):
        namefileanum = m2.group(1)
        namestring = m2.string[m2.end(1)+1:-1] # The rest, sans newline
        if(namefileanum >= anum):
          return((namefileanum,namestring))

    return(("unknwn","unknown"))


def anum_among_matches(anum,list_of_tuples):
  '''Check whether anum is present in the list of tuples.'''
  for t in list_of_tuples:
    (t_anum,t_name,t_case,t_num_of_eq_classes,t_sizediff_of_two_largest,t_size_of_second_largest_eq_class) = t
    if(anum == t_anum): return(True)

  return(False)


def append_to_list_if_anum_not_already_there(tuple,list_of_tuples):
  '''Appends tuple to list_of_tuples unless a tuple with the identical first elem is already there.'''

  (anum,name,case,num_of_eq_classes,sizediff_of_two_largest,size_of_second_largest_eq_class) = tuple

  for t in list_of_tuples:
    (t_anum,t_name,t_case,t_num_of_eq_classes,t_sizediff_of_two_largest,t_size_of_second_largest_eq_class) = t
    if(anum == t_anum): return(False)

  list_of_tuples.append(tuple)

  return(True)



def add_seq_to(possibly_matching_seqs,seq2,minlength_for_seq2,anum,name_used,seq1,seq1inverses,seq1_sizediff_of_two_largest,at_least_n_distinct_classes,max_diff_for_two_largest_classes,minsize_for_the_second_largest_eq_class):
  if(len(seq2) < minlength_for_seq2): return

  off = 0

  if(max_diff_for_two_largest_classes <= seq1_sizediff_of_two_largest):
    max_diff_for_two_largest_classes = 1+seq1_sizediff_of_two_largest

# seq2inverses = get_nonsingleton_inverses(seq2,off)
  seq2inverses = get_inverses(seq2,off)

  seq2_distinct_classes = has_coarser_eq_classes_with_threshold(seq2,off,seq1inverses,at_least_n_distinct_classes)

  if(seq2_distinct_classes >= at_least_n_distinct_classes):
    case = "=>"
    if(has_coarser_eq_classes_with_threshold(seq1,off,seq2inverses,at_least_n_distinct_classes)):
      case = "<=>"

    seq2invs_sorted = seq2inverses.items()
#   if(len(seq2invs_sorted) < 2): return

    seq2invs_sorted.sort(sort_eq_classes_by_size)
    (freq1val,freq1indices) = seq2invs_sorted[0]
    (freq2val,freq2indices) = seq2invs_sorted[1]
    sizediff_of_two_largest = len(freq1indices) - len(freq2indices)

    if(len(freq2indices) < minsize_for_the_second_largest_eq_class): return


    if(sizediff_of_two_largest <= max_diff_for_two_largest_classes):
      append_to_list_if_anum_not_already_there((anum,
                                                name_used,
                                                case,
                                                seq2_distinct_classes,
                                                sizediff_of_two_largest,
                                                len(freq2indices)),
                                               possibly_matching_seqs)
#     possibly_matching_seqs.append((anum,name_used,case,seq2_distinct_classes,sizediff_of_two_largest))


def read_bfile_as_a_list(filename):
  '''Open b-file for reading and construct a list from it.'''
  infp = open(filename,'r')

  linepat = re.compile(r'^( *[0-9]+) *[-]*([0-9]+)')

  terms = []

  for line in infp.xreadlines(): 
    m = linepat.match(line)
    if(m):
      ind  = int(m.group(1)) #
      val  = int(m.group(2)) #
      terms.append(val)
#   else:
#     print "SKIPPING THE FOLLOWING LINE in " + filename + ": " + line + "\n"

  infp.close()
  return(terms) # 



def eq_class_search_from_stripped_data(infilename_for_data,infilename_for_names,outfilename,seqname,seq1,except_not_these,minlength_for_seq2,at_least_n_distinct_classes,max_diff_for_two_largest_classes,minsize_for_the_second_largest_eq_class):
    '''Opens "infilename_for_data" for reading and writes results to "outfilename".'''
    outfp = open(outfilename,'a')

    linepat = re.compile(r'^([^ ]+) ')

    terms = []

    possibly_matching_seqs = []


    off = 0


    seq1 = rest(seq1)
    seq1inverses = get_nonsingleton_inverses(seq1,off)

    seq1rest = rest(seq1)
    seq1rest_inverses = get_nonsingleton_inverses(seq1rest,off)

    at_least_n_distinct_classes = min(at_least_n_distinct_classes,len(seq1inverses.items()))


    seq1invs_sorted = seq1inverses.items()
    seq1invs_sorted.sort(sort_eq_classes_by_size)
    (freq1val,freq1indices) = seq1invs_sorted[0]
    (freq2val,freq2indices) = seq1invs_sorted[1]
    seq1_sizediff_of_two_largest = len(freq1indices) - len(freq2indices)

    suspected_cases = 0
    namefileanum = ""

    blurp = "\nSearching matches for " + seqname + " from seqs with at least " + str(at_least_n_distinct_classes) + " equivalence classes and the second largest eq.class at least of size " + str(minsize_for_the_second_largest_eq_class) + ".\n"
    outfp.write(blurp)
    sys.stdout.write(blurp)

    infp = open(infilename_for_data,'r')
    infp2 = open(infilename_for_names,'r')

    for line in infp.xreadlines(): 
      m = linepat.match(line)

      if(m):
        anum  = m.group(1) #
#       sys.stdout.write(anum + "   " + str(suspected_cases) + "\r")

        if ('#' == anum): continue # Skip the comments in the beginning of stripped data

        if(anum_among_matches(anum,except_not_these)): continue # Skip the ones found from the black list.

# namefile could contain less entries than the data file (or in some cases more).
# in any case, in both files the entries should occur in the order sorted by their anums:
        if(namefileanum < anum):
          (namefileanum,namestring) = read_namefile_lines_until_anum_at_least_large_is_found(infp2,anum)

        if(namefileanum == anum):
          name_used = namestring
        else: 
          name_used = "(unknown)"

        contents = m.string[m.end(1)+1:-1] # The rest, sans newline
  
        seq2 = map(int,contents.replace(',',' ').split())

        add_seq_to(possibly_matching_seqs,rest(seq2),minlength_for_seq2,anum,name_used,seq1,seq1inverses,seq1_sizediff_of_two_largest,at_least_n_distinct_classes,max_diff_for_two_largest_classes,minsize_for_the_second_largest_eq_class)

        add_seq_to(possibly_matching_seqs,rest(seq2),minlength_for_seq2,anum,name_used,seq1rest,seq1rest_inverses,seq1_sizediff_of_two_largest,at_least_n_distinct_classes,max_diff_for_two_largest_classes,minsize_for_the_second_largest_eq_class)

        add_seq_to(possibly_matching_seqs,rest(rest(seq2)),minlength_for_seq2,anum,name_used,seq1,seq1inverses,seq1_sizediff_of_two_largest,at_least_n_distinct_classes,max_diff_for_two_largest_classes,minsize_for_the_second_largest_eq_class)


    infp.close()
# End of loop:


    possibly_matching_seqs.sort(sortseqsV1)

    for match in possibly_matching_seqs:
      (anum,name,case,num_of_eq_classes,sizediff_of_two_largest,size_of_second_largest_eq_class) = match
      outfp.write(seqname)
      outfp.write(" " + case + " (" + str(num_of_eq_classes) + "," + str(sizediff_of_two_largest) + "," + str(size_of_second_largest_eq_class) + ") " + anum + " " + name)
      suspected_cases += 1
      outfp.write("\n")

    outfp.write(seqname + ": total of " + str(suspected_cases) + " suspected matching sequences were found.\n")
    sys.stdout.write(seqname + ": total of " + str(suspected_cases) + " suspected matching sequences were found.\n")

    outfp.close()

    return(possibly_matching_seqs)



def rest(lista):
  '''Gives the rest of lista after its first element has been discarded. (Cf. cdr in Lisp and Scheme).'''
  return([lista[i] for i in range(1,len(lista))])

# "Master-sequence for RLT's": o=0:
A278222 = [1, 2, 2, 4, 2, 6, 4, 8, 2, 6, 6, 12, 4, 12, 8, 16, 2, 6, 6, 12, 6, 30, 12, 24, 4, 12, 12, 36, 8, 24, 16, 32, 2, 6, 6, 12, 6, 30, 12, 24, 6, 30, 30, 60, 12, 60, 24, 48, 4, 12, 12, 36, 12, 60, 36, 72, 8, 24, 24, 72, 16, 48, 32, 64, 2, 6, 6, 12, 6, 30, 12, 24, 6, 30, 30, 60, 12, 60, 24, 48, 6, 30, 30, 60, 30, 210, 60, 120, 12, 60, 60, 180, 24, 120, 48, 96, 4, 12, 12, 36, 12, 60, 36, 72, 12, 60, 60, 180, 36, 180, 72, 144, 8, 24, 24, 72, 24, 120, 72, 216, 16]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile, "A278222",A278222,30,2) #

## A127302 o=0: Matula-Goebel signatures for plane binary trees encoded by A014486.

A127302 = [1,4,14,14,86,86,49,86,86,886,886,454,886,886,301,301,301,886,886,301,454,886,886,13766,13766,6418,13766,13766,3986,3986,3986,13766,13766,3986,6418,13766,13766,3101,3101,1589,3101,3101,1849,1849,3101,13766]

## A153835 o=0: The first representative in A014486 for each equivalence class of non-oriented binary tree corresponding to the oriented (plane) binary tree encoded by A014486(n). 

A153835 = [0,1,2,2,4,4,6,4,4,9,9,11,9,9,14,14,14,9,9,14,11,9,9,23,23,25,23,23,28,28,28,23,23,28,25,23,23,37,37,39,37,37,42,42,37,23,23,37,25,23,23,42,42,39,28,28,37,28,23,23,37,28,25,23,23,65,65,67,65,65,70,70,70,65]


# A010051 o=1: Characteristic function of primes: 1 if n is prime else 0. 

A010051 = [0,1,1,0,1,0,1,0,0,0,1,0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,1,0]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A010051",A010051,30,4) #

A278236 = [1, 2, 2, 6, 4, 12, 2, 6, 6, 30, 12, 60, 4, 12, 12, 60, 36, 180, 8, 24, 24, 120, 72, 360, 2, 6, 6, 30, 12, 60, 6, 30, 30, 210, 60, 420, 12, 60, 60, 420, 180, 1260, 24, 120, 120, 840, 360, 2520, 4, 12, 12, 60, 36, 180, 12, 60, 60, 420, 180, 1260, 36, 180, 180, 1260, 900, 6300, 72, 360, 360, 2520, 1800, 12600, 8, 24, 24, 120, 72, 360, 24, 120, 120, 840, 360, 2520]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile, "A278236", A278236, 30, 2)


# A100260 o=1: Fixed point of morphism 0 -> 01, 1 -> 02, 2 -> 31, 3 -> 32.

A100260 = [0,1,0,2,0,1,3,1,0,1,0,2,3,2,0,2,0,1,0,2,0,1,3,1,3,2,3,1,0,1,3,1,0,1,0,2,0,1,3,1,0,1,0,2,3,2,0,2,3,2,3,1,3,2,0,2,0,1,0,2,3,2,0,2,0,1,0,2,0,1,3,1,0,1,0,2,3,2,0,2,0,1,0,2,0,1,3,1,3,2,3,1,0,1,3,1,3,2,3,1,3,2,0,2,3]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A100260",A100260,30,4) # Genuine match with A105584 ?

# A010060 o=0: Thue-Morse sequence: let A_k denote the first 2^k terms; then A_0 = 0 and for k >= 0, A_{k+1} = A_k B_k, where B_k is obtained from A_k by interchanging 0's and 1's.

A010060 = [0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1,1]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A010060",A010060,30,4) #



# A048675 o=1: If n = p_i^e_i * ... * p_k^e_k, p_i < ... < p_k primes (with p_1 = 2, p_2 = 3, ...), then a(n) = (1/2) * (e_i * 2^i + ... + e_k * 2^k).
A048675 = [0,1,2,2,4,3,8,3,4,5,16,4,32,9,6,4,64,5,128,6,10,17,256,5,8,33,6,10,512,7,1024,5,18,65,12,6,2048,129,34,7,4096,11,8192,18,8,257,16384,6,16,9,66,34,32768,7,20,11,130,513,65536,8,131072,1025,12,6,36,19]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A048675",A048675,30,5) # 5 cases, of which at least 2 - 3 are genuine.



# A055396 Smallest prime dividing n is a(n)-th prime (a(1)=0).

A055396 = [0,1,2,1,3,1,4,1,2,1,5,1,6,1,2,1,7,1,8,1,2,1,9,1,3,1,2,1,10,1,11,1,2,1,3,1,12,1,2,1,13,1,14,1,2,1,15,1,4,1,2,1,16,1,3,1,2,1,17,1,18,1,2,1,3,1,19,1,2,1,20,1,21,1,2,1,4,1,22,1,2,1,23,1,3,1,2,1,24,1,4,1,2,1,3,1]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A055396",A055396,30,4) # With threshold 6 10 suspected, most of them genuine? (Same with 4)

# A078898 [here o=1]: Number of times the smallest prime factor of n is the smallest prime factor for numbers <= n

A078898 = [0,1,1,2,1,3,1,4,2,5,1,6,1,7,3,8,1,9,1,10,4,11,1,12,2,13,5,14,1,15,1,16,6,17,3,18,1,19,7,20,1,21,1,22,8,23,1,24,2,25,9,26,1,27,4,28,10,29,1,30,1,31,11,32,5,33,1,34,12,35,1,36,1,37,13,38,3,39,1,40,14,41,1,42,6,43]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A078898",A078898,30,3) #  Only with itself?


##  A097246 o=1: Replace factors of n that are squares of a prime by the prime succeeding this prime.
##  Whenever A097246(i) = A097246(j) => A097248(j) = A097248(j).

A097246 = [1,2,3,3,5,6,7,6,5,10,11,9,13,14,15,9,17,10,19,15,21,22,23,18,7,26,15,21,29,30,31,18,33,34,35,15,37,38,39,30,41,42,43,33,25,46,47,27,11,14,51,39,53,30,55,42,57,58,59,45,61,62,35,27,65,66,67,51,69,70,71,30,73]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A097246",A097246,30,5) # 50 with threshold 3, 10 with threshold 5, with maybe 3 or 4 false positives.

##  A097248 o=1: a(n)=r(n,m) with m such that r(n,m)=r(n,m+1), where r(n,k)=A097246(r(n,k-1)), r(n,0)=n.

A097248 = [1,2,3,3,5,6,7,6,5,10,11,5,13,14,15,5,17,10,19,15,21,22,23,10,7,26,15,21,29,30,31,10,33,34,35,15,37,38,39,30,41,42,43,33,7,46,47,15,11,14,51,39,53,30,55,42,57,58,59,7,61,62,35,15,65,66,67,51,69,70,71,30,73,74,21]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A097248",A097248,30,5) # This gives ...



# A007913 o=1: Squarefree part of n: a(n) = smallest positive number m such that n/m is a square.
A007913 = [1,2,3,1,5,6,7,2,1,10,11,3,13,14,15,1,17,2,19,5,21,22,23,6,1,26,3,7,29,30,31,2,33,34,35,1,37,38,39,10,41,42,43,11,5,46,47,3,1,2,51,13,53,6,55,14,57,58,59,15,61,62,7,1,65,66,67,17,69,70,71,2,73,74,3,19,77]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A007913",A007913,30,5) #  23 cases, most of them relevant?






A000523 = [0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A000523",A000523,30,4) # 21 suspected cases, most of them genuine?

# A007814 Exponent of highest power of 2 dividing n, a.k.a. the binary carry sequence, the ruler sequence, or the 2-adic valuation of n.

A007814 = [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,6,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A007814",A007814,30,5) # 73 suspected

# A000523 o=1: a(n) = floor(log_2(n)). 

# A007949 o=1: Greatest k such that 3^k divides n. Or, 3-adic valuation of n. 

A007949 = [0,0,1,0,0,1,0,0,2,0,0,1,0,0,1,0,0,2,0,0,1,0,0,1,0,0,3,0,0,1,0,0,1,0,0,2,0,0,1,0,0,1,0,0,2,0,0,1,0,0,1,0,0,3,0,0,1,0,0,1,0,0,2,0,0,1,0,0,1,0,0,2,0,0,1,0,0,1,0,0,4,0,0,1,0,0,1,0,0,2,0,0,1,0,0,1,0,0,2,0,0,1,0,0,1]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A007949",A007949,30,4) # 8 suspected cases, most of them genuine?


# A002487 o=0: Stern's diatomic series (or Stern-Brocot sequence): a(0) = 0, a(1) = 1; for n > 0: a(2*n) = a(n), a(2*n+1) = a(n) + a(n+1).

A002487 = [0,1,1,2,1,3,2,3,1,4,3,5,2,5,3,4,1,5,4,7,3,8,5,7,2,7,5,8,3,7,4,5,1,6,5,9,4,11,7,10,3,11,8,13,5,12,7,9,2,9,7,12,5,13,8,11,3,10,7,11,4,9,5,6,1,7,6,11,5,14,9,13,4,15,11,18,7,17,10,13,3,14,11,19,8,21,13,18,5,17,12,19]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A002487",A002487,30,4) # Two matches, A002487 and A126606, both genuine.

# A277144 o=0: Lexicographically least sequence of nonnegative integers that avoids 5/4-powers.

A277144 = [0,0,0,0,1,1,1,1,0,2,0,2,1,0,1,0,0,1,0,1,1,2,1,2,0,0,0,0,1,3,1,1,0,1,0,2,1,0,1,3,0,2,0,0,1,1,1,1,0,0,0,2,1,2,1,0,0,1,0,1,1,0,1,2,0,2,0,0,1,3,1,1,0,0,0,3,1,1,1,0,0,2,0,1,1,0,1,3,0,1,0,0,1,2,1,1,0,0,0,2,1,1,1,0,0,3,0,1,1,0,1,2,0,1,0,0,1,3,1,1]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A277144",A277144,30,4) # Only with itself.


# A277314 o=0: Number of nonzero coefficients in Stern polynomial B(n,t).

A277314 = [0,1,1,2,1,2,2,3,1,3,2,3,2,3,3,4,1,4,3,3,2,3,3,4,2,4,3,4,3,4,4,5,1,5,4,4,3,4,3,4,2,4,3,4,3,4,4,5,2,5,4,4,3,4,4,5,3,5,4,5,4,5,5,6,1,6,5,5,4,5,4,5,3,5,4,4,3,4,4,5,2,5,4,4,3,4,4,5,3,5,4,5,4,5,5,6,2,6,5,5,4,5,4,5,3,5,4,5,4,5,5,6,3,6,5,5,4,5,5,6,4]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A277314",A277314,30,4) # Only with itself.

# A001175 o=1: Pisano periods (or Pisano numbers): period of Fibonacci numbers mod n.

A001175 = [1,3,8,6,20,24,16,12,24,60,10,24,28,48,40,24,36,24,18,60,16,30,48,24,100,84,72,48,14,120,30,48,40,36,80,24,76,18,56,60,40,48,88,30,120,48,32,24,112,300,72,84,108,72,20,48,72,42,58,120,60,30,48,96,140,120,136]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A001175",A001175,30,8) # With threshold 4, 58 suspected cases, with 8 total 3 suspected cases, at least 2 genuine.

# A000001 [Here o=1] Number of groups of order n.

A000001 = [1,1,1,2,1,2,1,5,2,2,1,5,1,2,1,14,1,5,1,5,2,2,1, 15,2,2,5,4,1,4,1,51,1,2,1,14,1,2,2,14,1,6,1,4,2,2, 1,52,2,5,1,5,1,15,2,13,2,2,1,13,1,2,4,267,1,4,1,5, 1,4,1,50,1,2,3,4,1,6,1,52,15,2,1,15,1,2,1,12,1,10, 1,4,2]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A000001",A000001,30,4) # No matches.


A001221 = [0,1,1,1,1,2,1,1,1,2,1,2,1,2,2,1,1,2,1,2,2,2,1,2,1,2,1,2,1,3,1,1,2,2,2,2,1,2,2,2,1,3,1,2,2,2,1,2,1,2,2,2,1,2,2,2,2,2,1,3,1,2,2,1,2,3,1,2,2,3,1,2,1,2,2,2,2,3,1,2,1,2,1,3,2,2,2,2,1,3,2,2,2,2,2,2,1,2,2,2,1,3,1,2,3,2,1,2,1,3,2]

A001222 = [0,1,1,2,1,2,1,3,2,2,1,3,1,2,2,4,1,3,1,3,2,2,1,4,2,2,3,3,1,3,1,5,2,2,2,4,1,2,2,4,1,3,1,3,3,2,1,5,2,3,2,3,1,4,2,4,2,2,1,4,1,2,3,6,2,3,1,3,2,3,1,5,1,2,3,3,2,3,1,5,4,2,1,4,2,2,2,4,1,4,2,3,2,2,2,6,1,3,3,4,1,3,1,4,3,2,1,5,1,3,2]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A001221",A001221,30,4) #


##  A046523 o=1: Smallest number with same prime signature as n. 
##  We have whenever for any i,j: A046523(i) = A046523(j) => A001221(i) = A001221(j)
##  and also A046523(i) = A046523(j) => A001222(i) = A001222(j)
A046523 = [1,2,2,4,2,6,2,8,4,6,2,12,2,6,6,16,2,12,2,12,6,6,2,24,4,6,8,12,2,30,2,32,6,6,6,36,2,6,6,24,2,30,2,12,12,6,2,48,4,12,6,12,2,24,6,24,6,6,2,60,2,6,12,64,6,30,2,12,6,30,2,72,2,6,12,12,6,30,2,48,16,6,2,60,6,6,6,24,2]

# eq_class_search_from_stripped_data(datafile, namefile, outputfile,"A046523",A046523,30,5) # 376 with threshold 5, 495 with threshold 3.




def count_almost_injections_from_stripped_data(filename,outfilename,skipnfirst):
    '''Opens "filenames" for reading and writes results to "outfilename".'''
    outfp = open(outfilename,'w')

    linepat = re.compile(r'^([^ ]+) ')

    terms = []

    too_shorts = 0
    suspected_injections = 0
    suspected_non_injections = 0
    suspected_monotonic_noninjections = 0

    sys.stdout.write("--- Opening " + filename + " ---\n")
    infp = open(filename,'r')

    for line in infp.xreadlines(): 
      m = linepat.match(line)
      if(m):
        anum  = m.group(1) #
#       sys.stdout.write(anum + "   " + str(suspected_injections).zfill(6) + "   " + str(suspected_non_injections).zfill(6) + "   " + str(too_shorts).zfill(6) + "\r")

        if ('#' == anum): continue # Skip the comments in the beginning of stripped data

        contents = m.string[m.end(1)+1:-1] # The rest, sans newline
  
        terms = map(int,contents.replace(',',' ').split())

        if(len(terms) <= skipnfirst):
          too_shorts = 1+too_shorts
          continue

###     outfp.write(anum + ": terms[0]=" + str(terms[0]) + ", terms[" + str(len(terms)-1) + "]=" + str(terms[len(terms)-1]) + "\n");

        ihash = {}

        is_not_injective = 0
        nonmonotonic = 0 # Unless a counter-example found.

        prevterm = terms[skipnfirst-1]

        for term in terms[skipnfirst:]:
          if(term < prevterm): nonmonotonic = 1
          prevterm = term
          if term in ihash:
            is_not_injective = 1
          else:
            ihash[term] = 1


        if(0 == is_not_injective):
          suspected_injections = 1+suspected_injections
        else:
          suspected_non_injections = 1+suspected_non_injections
          if(0 == nonmonotonic):
            suspected_monotonic_noninjections = 1+suspected_monotonic_noninjections
          else:
            outfp.write(anum + " " + contents)
            outfp.write("\n")

  
  
        terms = []
  
  #   else: Something else, just skip!
    infp.close()
# End of loop: for filename in filenames:

    sys.stdout.write("\n------------- Summary follows -------------\n")
    sys.stdout.write("Total of " + str(suspected_injections) + " + " + str(suspected_non_injections) + " suspected injections & non-injections were found, of which latter " + str(suspected_monotonic_noninjections) + " suspected monotonic sequences were also filtered off, leaving " + str(suspected_non_injections-suspected_monotonic_noninjections) + " nonmonotonic noninjections.\n")

    outfp.close()



def count_nonmonotonic_injections_from_stripped_data(filename,outfilename,skipnfirst,length_at_least):
    '''Opens "filenames" for reading and writes results to "outfilename".'''
    outfp = open(outfilename,'w')

    linepat = re.compile(r'^([^ ]+) ')

    terms = []

    
    too_shorts = 0
    suspected_injections = 0
    suspected_non_injections = 0
    suspected_monotonic_injections = 0

    sys.stdout.write("--- Opening " + filename + " ---\n")
    infp = open(filename,'r')

    for line in infp.xreadlines(): 
      m = linepat.match(line)
      if(m):
        anum  = m.group(1) #
#       sys.stdout.write(anum + "   " + str(suspected_injections).zfill(6) + "   " + str(suspected_non_injections).zfill(6) + "   " + str(too_shorts).zfill(6) + "\r")

        if ('#' == anum): continue # Skip the comments in the beginning of stripped data

        contents = m.string[m.end(1)+1:-1] # The rest, sans newline
  
        terms = map(int,contents.replace(',',' ').split())

        if(len(terms) <= length_at_least):
          too_shorts = 1+too_shorts
          continue

###     outfp.write(anum + ": terms[0]=" + str(terms[0]) + ", terms[" + str(len(terms)-1) + "]=" + str(terms[len(terms)-1]) + "\n");

        ihash = {}

        is_not_injective = 0
        nonmonotonic = 0 # Unless a counter-example found.

        prevterm = terms[skipnfirst-1]

        for term in terms[skipnfirst:]:
          if(term < prevterm): nonmonotonic = 1
          prevterm = term
          if term in ihash:
            is_not_injective = 1
          else:
            ihash[term] = 1


        if(0 == is_not_injective):
          suspected_injections = 1+suspected_injections
          if(0 == nonmonotonic):
            suspected_monotonic_injections = 1+suspected_monotonic_injections
          else:
            outfp.write(anum + " " + contents)
            outfp.write("\n")
        else:
          suspected_non_injections = 1+suspected_non_injections

  
  
        terms = []
  
  #   else: Something else, just skip!
    infp.close()
# End of loop: for filename in filenames:

    sys.stdout.write("\n------------- Summary follows -------------\n")
    sys.stdout.write("Total of " + str(suspected_injections) + " + " + str(suspected_non_injections) + " suspected injections & non-injections were found, of which former " + str(suspected_monotonic_injections) + " suspected monotonic sequences were also filtered off, leaving " + str(suspected_injections-suspected_monotonic_injections) + " nonmonotonic injections.\n")

    outfp.close()


def search_mult_div_permutations_from_stripped_data(infilename_for_data,infilename_for_names,outfilename,except_not_these):
    '''Opens "infilename_for_data" for reading and writes results to "outfilename".'''
    outfp = open(outfilename,'a')

    linepat = re.compile(r'^([^ ]+) ')

    terms = []

    namefileanum = ""

    infp = open(infilename_for_data,'r')
    infp2 = open(infilename_for_names,'r')

    suspected_cases = 0

    for line in infp.xreadlines(): 
      m = linepat.match(line)

      if(m):
        anum  = m.group(1) #

        if ('#' == anum): continue # Skip the comments in the beginning of stripped data

        if(anum_among_matches(anum,except_not_these)): continue # Skip the ones found from the black list.

# namefile could contain less entries than the data file (or in some cases more).
# in any case, in both files the entries should occur in the order sorted by their anums:
        if(namefileanum < anum):
          (namefileanum,namestring) = read_namefile_lines_until_anum_at_least_large_is_found(infp2,anum)

        if(namefileanum == anum):
          name_used = namestring
        else: 
          name_used = "(unknown)"

        contents = m.string[m.end(1)+1:-1] # The rest, sans newline
  
        terms = map(int,contents.replace(',',' ').split())

        prevterm = 1
        satisfies_the_condition = 1

        for term in terms[1:]:
          if((term<1) or ((term % prevterm) and (prevterm % term))):
            satisfies_the_condition = 0
            break
          prevterm = term


        if(satisfies_the_condition):
          outfp.write(anum + " " + name_used)
          outfp.write("\n")
          suspected_cases += 1
          

    infp.close()
# End of loop:

    sys.stdout.write("Total of " + str(suspected_cases) + " suspected matching sequences were found.\n")

    outfp.close()



# Comment the following line off after the non-injections file has been prepared:
# count_nonmonotonic_injections_from_stripped_data(datafile,"./nonmonotonic-injections.txt",1,25)

# search_mult_div_permutations_from_stripped_data("./nonmonotonic-injections.txt",namefile,"./mult-div-permutations.txt",[])


def search_single_bit_change_permutations_from_stripped_data(infilename_for_data,infilename_for_names,outfilename,except_not_these):
    '''Opens "infilename_for_data" for reading and writes results to "outfilename".'''
    outfp = open(outfilename,'a')

    linepat = re.compile(r'^([^ ]+) ')

    terms = []

    namefileanum = ""

    infp = open(infilename_for_data,'r')
    infp2 = open(infilename_for_names,'r')

    suspected_cases = 0

    for line in infp.xreadlines(): 
      m = linepat.match(line)

      if(m):
        anum  = m.group(1) #

        if ('#' == anum): continue # Skip the comments in the beginning of stripped data

        if(anum_among_matches(anum,except_not_these)): continue # Skip the ones found from the black list.

# namefile could contain less entries than the data file (or in some cases more).
# in any case, in both files the entries should occur in the order sorted by their anums:
        if(namefileanum < anum):
          (namefileanum,namestring) = read_namefile_lines_until_anum_at_least_large_is_found(infp2,anum)

        if(namefileanum == anum):
          name_used = namestring
        else: 
          name_used = "(unknown)"

        contents = m.string[m.end(1)+1:-1] # The rest, sans newline
  
        terms = map(int,contents.replace(',',' ').split())

        prevterm = terms[0]
        satisfies_the_condition = 1

        for term in terms[1:]:
          if((term < 0) or (bin(term^prevterm).count('1') > 1)):
            satisfies_the_condition = 0
            break
          prevterm = term


        if(satisfies_the_condition):
          outfp.write(anum + " " + name_used)
          outfp.write("\n")
          suspected_cases += 1
          

    infp.close()
# End of loop:

    sys.stdout.write("Total of " + str(suspected_cases) + " suspected matching sequences were found.\n")

    outfp.close()



# Comment the following line off after the non-injections file has been prepared:
# count_nonmonotonic_injections_from_stripped_data(datafile,"./nonmonotonic-injections.txt",1,25)

# search_single_bit_change_permutations_from_stripped_data("./nonmonotonic-injections.txt",namefile,"./suspected-single-bit-change-permutations.txt",[])


def main_loop_over_datafile(selected_anums,datafile,namefile,outfilename,at_least_n_distinct_classes,max_diff_for_two_largest_classes,minlength_for_seq2,minsize_for_the_second_largest_eq_class):
    '''Opens "datefilename" for reading in the main loop and writes results to "outfilename".'''


    except_not_these = []

    tot_suspected_cases = 0



    linepat = re.compile(r'^([^ ]+) ')

    for anum in selected_anums:
      sys.stdout.write("--- Opening " + datafile + " ---\n")
      infp = open(datafile,'r')
  
      for line in infp.xreadlines(): 
        m = linepat.match(line)
        if(m):
          if(anum != m.group(1)): continue
  
          contents = m.string[m.end(1)+1:-1] # The rest, sans newline
    
          terms = map(int,contents.replace(',',' ').split())
  
          matches = eq_class_search_from_stripped_data(datafile,namefile,outfilename,anum,terms,except_not_these,minlength_for_seq2,at_least_n_distinct_classes,max_diff_for_two_largest_classes,minsize_for_the_second_largest_eq_class)
          tot_suspected_cases = tot_suspected_cases + len(matches)

          except_not_these = except_not_these + matches
    
    #   else: Something else, just skip!
      infp.close()
#   End of loop: for filename in filenames:


#   outfp = open(outfilename,'a')

    sys.stdout.write("\n------------- Summary follows -------------\n")
    sys.stdout.write("Total of " + str(tot_suspected_cases) + " were found.\n")

#   outfp.close()



main()

# This module ends here.

