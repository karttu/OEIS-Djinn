#!/usr/bin/python3

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
# Should run also in cloud.sagemath.com (now cocalc.com)
#
# Last edited 2025-01-12 by Antti Karttunen

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
# 2019-02-09   And again corrected the very same function, so now the negative terms really are read as negative numbers.
#
# 2021-11-14   Added "usage notes" to the beginning of list selected_anums, when used without any arguments.
#              (Grep for global variable outputfile below. To get help for argumented usage, start with option --help).
#              PLEASE note that the signal / noise ratio of the results greatly depend on the "filter sequence(s)" that
#              are used, and also the number of terms available for each sequence in the stripped.gz data file.
#              For sequence A101296 the s/n ratio is quite good, for A295300 much less so.
#              Also, in many cases what is shown as "<=>" relation is actually "=>" relation, or no relation at all,
#              because the short beginnings of the sequences are not enough for telling the difference.
#
#              In any case, in the grand scheme of things, the purpose of this script is just to preselect
#              the sequences for which the b-files will need to be created (if not in OEIS already), after which
#              the real tests can be done with much larger amount of data, resulting much better s/n ratio (hopefully!)
#              than with this simple Python script.
#              See e.g., https://github.com/karttu/OEIS-Djinn/blob/master/src/OEIS-Djinn-on-SQL-find-links.rkt
#              for some development.
#
# 2021-11-24   Added the list my_slec1_filters for including some of the special filters in the results, even if slec is set to 2.
#
# 2021-11-25   Added bfile checking from bfilelist. Whether b-file exists for the matched sequence is indicated with + or - in
#              the beginning of each output line.
#
# 2021-11-28   Now also counts the total bfile size used by each set of sequenced matched to. Cleaned off lots of commented out old test code.
#
# 2025-01-12   Added A379000 to my_slec1_filters list.
# 2025-01-19   Added A127648 to my_slec1_filters list.
#
# 2025-01-21   Ported the code to Python 3. It's funny those clowns cannot make anymore programming languages
#              that would survive more than a quarter of century...
#
#

import os
import re
import sys
import functools

org_datafile = "./stripped"
datafile = "./non-injections.txt"
namefile = "./names"

at_least_n_distinct_classes = 2
max_diff_for_two_largest_classes = 150 # 
minlength_for_seq2 = 30
minsize_for_the_second_largest_eq_class = 2 # Usually best to be at least 2.

singleton_classes_also = True # If true, then also matches with only singleton equivalence classes are considered


def main():
  bfiles = read_bfilelist("bfilelist")

  if(len(sys.argv) > 1):
    if("--help" == sys.argv[1]):
      sys.stdout.write("Usage: " + sys.argv[0] + "\n   For ordinary usage involving stripped and names downloaded from https://oeis.org/wiki/Welcome#Compressed_Versions start without any arguments.\n\n")
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
        print(result)
      print("---")
      print("")
  else: # No args
    if((not os.path.exists(datafile)) or (os.path.exists(org_datafile) and (os.stat(datafile).st_mtime < os.stat(org_datafile).st_mtime))):
      if(not os.path.exists(datafile)): sys.stdout.write("File " + datafile + " does not exist, preparing it from " + org_datafile + "\n")
      else: sys.stdout.write("File " + datafile + " exists, but is out of date, repreparing from " + org_datafile + "\n")
      if(not os.path.exists(org_datafile)):
        sys.stdout.write("File " + org_datafile + " does not exist either, please download it with command:\n  wget https://oeis.org/stripped.gz\nand then\n  gunzip stripped.gz\nDo the same with https://oeis.org/names.gz\n")
        exit(1)

      
      count_almost_injections_from_stripped_data(org_datafile,datafile,8)

    main_loop_over_datafile(selected_anums,datafile,namefile,outputfile,at_least_n_distinct_classes,max_diff_for_two_largest_classes,minlength_for_seq2,minsize_for_the_second_largest_eq_class,bfiles)

# main ends here.
    

# This list contains all such "filter sequences" whose second largest eq.class is 1 (that is, sequences that have just one big eq.class, and all the rest are singletons),
# but which are seqs I still want to be included in the results, even if minsize_for_the_second_largest_eq_class above is set to value larger than 1:
my_slec1_filters = ["A127648", "A305800", "A305801", "A305890", "A305900", "A305976", "A305979", "A305980", "A319701", "A319702", "A320014", "A322810", "A326201", "A326202", "A326203", "A346488", "A379000"]


outputfile = "./output_for_A379000_with_singletons_and_slec2_2025-01-21.txt" # This is the file name where the output goes when the script is started without any arguments. It is better to correspond with the A-numbers mentioned in the beginning of the selected_anums structure below:



selected_anums = [

# If multiple A-numbers are given, then the most exclusive should come first, and after each subsequent A-number
# the "total of X suspected matching sequences were found" count excludes the previous matches. For example, the second
# count here will be then the cardinality of the {setwise difference matches to A295300 \ matches to A101296}, and
# the third count will be the cardinality of  the {setwise difference matches to A305801 \ matches to A295300}.
# NOTE: if this doesn't work as you expect, check that all the active filter Anumbers are found in non-injections.txt
# If the problem persists (e.g., that file has been cut prematurely), then rm non-injections.txt and run again.
#
# Comments indicate the situation as of Jan 21 2025.

"A007814", # (593,  400 without b-file, don't care about them) the 2-adic valuation, used to filter chaff off from the results of later filters
"A130909", # (376,  274 without b-file, don't care about them) Simple periodic sequence (n mod 16), used just to filter chaff (also all period 2, 4 and 8 seqs) of from the results of filters below
"A010881", # (664,  505) n mod 12, we are not interested about the period-3 or period-6 or period-12 sequences.
"A101296", # (1443, 286 still without a b-file) All sequences that depend only the prime signature of n. (A101296 is the RGS-transform of A046523, Smallest number with same prime signature as n).
"A378601", # 43/4
"A378602", # 90/10
"A378603", # 7/0
"A378604", # 2/0
"A378605", # 231/54
"A305800", # (4264, 722 without a b-file)  All sequences x for which x(p) = constant for all primes (including 2).
"A305801", # (1434, 181 without a b-file)  All sequences x for which x(p) = constant for all odd primes should match to this one, thus including the above sets and also many, many others.
"A322026", # (46,   6 without a b-file)
"A127648", # 34/3, "triangularizations", triangles with nonzero terms only on the leading or trailing diagonal.
"A305900", # (686,  117 without a b-file).
"A379005", # 64/36
"A379001", # 39/19
"A379000"  # (247,  100 without a b-file).  Total 10251 matches, 7429 with a real b-file (not synthesized).

]

selected_anums_for_A003602 = [
  "A003602" # (1635 suspected matches, and 427 without b-file, as on Jan 12 2025)
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

      elif seq1[k-off] != e: # found that seq1 obtains a different value at some point in the same e.c. of seq2,
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

def read_bfilelist(filename):
  '''Open bfilelist for reading and construct a list from the fifth field in it.'''
  infp = open(filename,'r')

  linepat = re.compile(r'^\s*([0-9]+)\s*[^\s]*\s*[^\s]*\s*[^\s]*\s*b([0-9]+)\.') # Last edited 2021-11-25.

  bfiles = {}

  for line in infp.readlines(): # was xreadlines in Python2 
    m = linepat.match(line)
    if(m):
      size  = int(m.group(1)) #
      anum  = m.group(2) #
      bfiles[("A"+anum)] = size
#   else:
#     print "SKIPPING THE FOLLOWING LINE in " + filename + ": " + line + "\n"

  infp.close()
  return(bfiles) # 



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

    seq2invs_sorted = list(seq2inverses.items())
#   if(len(seq2invs_sorted) < 2): return

    seq2invs_sorted.sort(key=functools.cmp_to_key(sort_eq_classes_by_size)) # Python3 "fix"
    (freq1val,freq1indices) = seq2invs_sorted[0]
    (freq2val,freq2indices) = seq2invs_sorted[1]
    sizediff_of_two_largest = len(freq1indices) - len(freq2indices)

    if((len(freq2indices) < minsize_for_the_second_largest_eq_class) and not(anum in my_slec1_filters)): return


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

  linepat = re.compile(r'^(\s*[0-9]+)\s*([-]*[0-9]+)') # Last edited 2019-02-09.

  terms = []

  for line in infp.readlines(): # was xreadlines in Python2 
    m = linepat.match(line)
    if(m):
      ind  = int(m.group(1)) #
      val  = int(m.group(2)) #
      terms.append(val)
#   else:
#     print "SKIPPING THE FOLLOWING LINE in " + filename + ": " + line + "\n"

  infp.close()
  return(terms) # 



def eq_class_search_from_stripped_data(infilename_for_data,infilename_for_names,outfilename,seqname,seq1,except_not_these,minlength_for_seq2,at_least_n_distinct_classes,max_diff_for_two_largest_classes,minsize_for_the_second_largest_eq_class,bfiles):
    '''Opens "infilename_for_data" for reading and writes results to "outfilename".'''
    outfp = open(outfilename,'a')

    linepat = re.compile(r'^([^ ]+) ')

    terms = []

    possibly_matching_seqs = []

    bfsiztot = nobfile = 0

    off = 0


    seq1 = rest(seq1)
    seq1inverses = get_nonsingleton_inverses(seq1,off)

    seq1rest = rest(seq1)
    seq1rest_inverses = get_nonsingleton_inverses(seq1rest,off)

    at_least_n_distinct_classes = min(at_least_n_distinct_classes,len(seq1inverses.items()))


    seq1invs_sorted = list(seq1inverses.items())
    seq1invs_sorted.sort(key=functools.cmp_to_key(sort_eq_classes_by_size)) # Python3 "fix"
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

    for line in infp.readlines(): # was xreadlines in Python2 
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
  
        seq2 = list(map(int,contents.replace(',',' ').split())) # list around map required by Python3

        add_seq_to(possibly_matching_seqs,rest(seq2),minlength_for_seq2,anum,name_used,seq1,seq1inverses,seq1_sizediff_of_two_largest,at_least_n_distinct_classes,max_diff_for_two_largest_classes,minsize_for_the_second_largest_eq_class)

        add_seq_to(possibly_matching_seqs,rest(seq2),minlength_for_seq2,anum,name_used,seq1rest,seq1rest_inverses,seq1_sizediff_of_two_largest,at_least_n_distinct_classes,max_diff_for_two_largest_classes,minsize_for_the_second_largest_eq_class)

        add_seq_to(possibly_matching_seqs,rest(rest(seq2)),minlength_for_seq2,anum,name_used,seq1,seq1inverses,seq1_sizediff_of_two_largest,at_least_n_distinct_classes,max_diff_for_two_largest_classes,minsize_for_the_second_largest_eq_class)


    infp.close()
# End of loop:


    possibly_matching_seqs.sort(key=functools.cmp_to_key(sortseqsV1)) # Python3 "fix"


    for match in possibly_matching_seqs:
      (anum,name,case,num_of_eq_classes,sizediff_of_two_largest,size_of_second_largest_eq_class) = match
      if(anum in bfiles):
        bfsiztot += bfiles[anum]
        outfp.write("+ ")
      else:
        nobfile += 1
        outfp.write("- ")
      outfp.write(seqname)
      outfp.write(" " + case + " (" + str(num_of_eq_classes) + "," + str(sizediff_of_two_largest) + "," + str(size_of_second_largest_eq_class) + ") " + anum + " " + name)
      suspected_cases += 1
      outfp.write("\n")

    blurp = (seqname + ": total of " + str(suspected_cases) + " suspected matching sequences were found, with bfile sizes totaling "+ str(bfsiztot) + ", and " + str(nobfile) + " without a b-file.\n")
    outfp.write(blurp)
    sys.stdout.write(blurp)

    outfp.close()

    return(possibly_matching_seqs)



def rest(lista):
  '''Gives the rest of lista after its first element has been discarded. (Cf. cdr in Lisp and Scheme).'''
  return([lista[i] for i in range(1,len(lista))])



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

    for line in infp.readlines(): # was xreadlines in Python2 
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

    for line in infp.readlines(): # was xreadlines in Python2 
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

    for line in infp.readlines(): # was xreadlines in Python2 
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

    for line in infp.readlines(): # was xreadlines in Python2 
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


def main_loop_over_datafile(selected_anums,datafile,namefile,outfilename,at_least_n_distinct_classes,max_diff_for_two_largest_classes,minlength_for_seq2,minsize_for_the_second_largest_eq_class,bfiles):
    '''Opens "datefilename" for reading in the main loop and writes results to "outfilename".'''


    except_not_these = []

    tot_suspected_cases = 0



    linepat = re.compile(r'^([^ ]+) ')

    for anum in selected_anums:
      sys.stdout.write("--- Opening " + datafile + " ---\n")
      infp = open(datafile,'r')
  
      for line in infp.readlines(): # was xreadlines in Python2 
        m = linepat.match(line)
        if(m):
          if(anum != m.group(1)): continue
  
          contents = m.string[m.end(1)+1:-1] # The rest, sans newline
    
          terms = list(map(int,contents.replace(',',' ').split())) # list around map required by Python3
  
          matches = eq_class_search_from_stripped_data(datafile,namefile,outfilename,anum,terms,except_not_these,minlength_for_seq2,at_least_n_distinct_classes,max_diff_for_two_largest_classes,minsize_for_the_second_largest_eq_class,bfiles)
          tot_suspected_cases += len(matches)

          except_not_these += matches
    
    #   else: Something else, just skip!
      infp.close()
#   End of loop: for filename in filenames:


#   outfp = open(outfilename,'a')

    sys.stdout.write("\n------------- Summary follows -------------\n")
    sys.stdout.write("Total of " + str(tot_suspected_cases) + " were found. Output written to file: " + outfilename + "\n")

#   outfp.close()


main()

# This module ends here.

