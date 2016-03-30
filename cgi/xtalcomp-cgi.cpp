/***************** start of the getcgivars() module **********************/

/*************************************************************************/
/**                                                                     **/
/**     getcgivars.c-- routine to read CGI input variables into an      **/
/**         array of strings.                                           **/
/**                                                                     **/
/**     Written in 1996 by James Marshall, james@jmarshall.com, except  **/
/**     that the x2c() and unescape_url() routines were lifted directly **/
/**     from NCSA's sample program util.c, packaged with their HTTPD.   **/
/**                                                                     **/
/**     For the latest, see http://www.jmarshall.com/easy/cgi/ .        **/
/**                                                                     **/
/*************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/** Convert a two-char hex string into the char it represents. **/
char x2c(char *what) {
  register char digit;

  digit = (what[0] >= 'A' ? ((what[0] & 0xdf) - 'A')+10 : (what[0] - '0'));
  digit *= 16;
  digit += (what[1] >= 'A' ? ((what[1] & 0xdf) - 'A')+10 : (what[1] - '0'));
  return(digit);
}

/** Reduce any %xx escape sequences to the characters they represent. **/
void unescape_url(char *url) {
  register int i,j;

  for(i=0,j=0; url[j]; ++i,++j) {
    if((url[i] = url[j]) == '%') {
      url[i] = x2c(&url[j+1]) ;
      j+= 2 ;
    }
  }
  url[i] = '\0' ;
}


/** Read the CGI input and place all name/val pairs into list.        **/
/** Returns list containing name1, value1, name2, value2, ... , NULL  **/
char **getcgivars() {
  register int i ;
  char *request_method ;
  int content_length;
  char *cgiinput ;
  char **cgivars ;
  char **pairlist ;
  int paircount ;
  char *nvpair ;
  char *eqpos ;

  /** Depending on the request method, read all CGI input into cgiinput. **/
  request_method= getenv("REQUEST_METHOD") ;

  if (!strcmp(request_method, "GET") || !strcmp(request_method, "HEAD") ) {
    /* Some servers apparently don't provide QUERY_STRING if it's empty, */
    /*   so avoid strdup()'ing a NULL pointer here.                      */
    char *qs ;
    qs= getenv("QUERY_STRING") ;
    cgiinput= strdup(qs  ? qs  : "") ;
  }
  else if (!strcmp(request_method, "POST")) {
    /* strcasecmp() is not supported in Windows-- use strcmpi() instead */
    if ( strcasecmp(getenv("CONTENT_TYPE"), "application/x-www-form-urlencoded")) {
      printf("Content-Type: text/plain\n\n") ;
      printf("getcgivars(): Unsupported Content-Type.\n") ;
      exit(1) ;
    }
    if ( !(content_length = atoi(getenv("CONTENT_LENGTH"))) ) {
      printf("Content-Type: text/plain\n\n") ;
      printf("getcgivars(): No Content-Length was sent with the POST request.\n") ;
      exit(1) ;
    }
    if ( !(cgiinput= (char *) malloc(content_length+1)) ) {
      printf("Content-Type: text/plain\n\n") ;
      printf("getcgivars(): Couldn't malloc for cgiinput.\n") ;
      exit(1) ;
    }
    if (!fread(cgiinput, content_length, 1, stdin)) {
      printf("Content-Type: text/plain\n\n") ;
      printf("getcgivars(): Couldn't read CGI input from STDIN.\n") ;
      exit(1) ;
    }
    cgiinput[content_length]='\0' ;
  }
  else {
    printf("Content-Type: text/plain\n\n") ;
    printf("getcgivars(): Unsupported REQUEST_METHOD.\n") ;
    exit(1) ;
  }

  /** Change all plusses back to spaces. **/
  for (i=0; cgiinput[i]; i++) if (cgiinput[i] == '+') cgiinput[i] = ' ' ;

  /** First, split on "&" and ";" to extract the name-value pairs into **/
  /**   pairlist.                                                      **/
  pairlist= (char **) malloc(256*sizeof(char **)) ;
  paircount= 0 ;
  nvpair= strtok(cgiinput, "&;") ;
  while (nvpair) {
    pairlist[paircount++]= strdup(nvpair) ;
    if (!(paircount%256))
      pairlist= (char **) realloc(pairlist,(paircount+256)*sizeof(char **)) ;
    nvpair= strtok(NULL, "&;") ;
  }
  pairlist[paircount]= 0 ;    /* terminate the list with NULL */

  /** Then, from the list of pairs, extract the names and values. **/
  cgivars= (char **) malloc((paircount*2+1)*sizeof(char **)) ;
  for (i= 0; i<paircount; i++) {
    if (eqpos=strchr(pairlist[i], '=')) {
      *eqpos= '\0' ;
      unescape_url(cgivars[i*2+1]= strdup(eqpos+1)) ;
    } else {
      unescape_url(cgivars[i*2+1]= strdup("")) ;
    }
    unescape_url(cgivars[i*2]= strdup(pairlist[i])) ;
  }
  cgivars[paircount*2]= 0 ;   /* terminate the list with NULL */

  /** Free anything that needs to be freed. **/
  free(cgiinput) ;
  for (i=0; pairlist[i]; i++) free(pairlist[i]) ;
  free(pairlist) ;

  /** Return the list of name-value strings. **/
  return cgivars ;

}

/***************** end of the getcgivars() module ********************/

#include "../xtalcomp.h"

#include <cassert>
#include <iostream>
#include <sstream>
#include <string>

#define PRINT_DIV \
  printf("|-------------------------------------|-------------------------------------|\n")

#define SUPERCELL_BUILDER_DEBUG

bool parsePOSCAR(char *, XcMatrix&, std::vector<XcVector>&, std::vector<unsigned int>&);

std::string debug;

// Counts the number of times 'val' occurs in a
// vector of that type
template <typename T>
uint countNumOccurrences(const std::vector<T>& vec,
                         const T& val)
{
  uint numOccurrences = 0;
  for (size_t i = 0; i < vec.size(); i++) {
    if (vec.at(i) == val) numOccurrences++;
  }
  return numOccurrences;
}

uint getFormulaUnits(const std::vector<uint>& types)
{
  // Find the atom type with the smallest quantity
  uint smallestQuantity = 10000000;
  for (size_t i = 0; i < types.size(); i++) {
    uint tmp = countNumOccurrences<uint>(types, types.at(i));
    if (tmp < smallestQuantity) smallestQuantity = tmp;
  }

  uint formulaUnits = 1;
  bool formulaUnitsFound;
  // Start with the smallest quantity and work our way down to 1
  for (size_t i = smallestQuantity; i > 1; i--) {
    formulaUnitsFound = true;
    for (size_t j = 0; j < types.size(); ++j) {
      // If we find any cases where there is a remainder after dividing, then
      // this is not the correct number of formula units
      if (countNumOccurrences<uint>(types, types.at(j)) % i != 0) formulaUnitsFound = false;
    }
    if (formulaUnitsFound == true) {
      formulaUnits = i;
      break;
    }
  }
  return formulaUnits;
}

// Counts the number of each type and stores them in the same
// order in a vector
// For example, if the input vector is "0, 0, 0, 1, 1, 2",
// the output vector is "3, 2, 1"
template <typename T>
std::vector<uint> countNumOfEachType(const std::vector<T>& types)
{
  std::vector<uint> ret;
  std::vector<T> alreadyCounted;
  for (size_t i = 0; i < types.size(); i++) {
    // Check and make sure we haven't already counted this one
    bool alreadyDone = false;
    for (size_t j = 0; j < alreadyCounted.size(); j++) {
      if (alreadyCounted.at(j) == types.at(i)) {
        alreadyDone = true;
        break;
      }
    }
    if (alreadyDone) continue;

    // Let's count how many times this occurs!
    uint numOccurrences = countNumOccurrences<T>(types, types.at(i));
    ret.push_back(numOccurrences);
    alreadyCounted.push_back(types.at(i));
  }

  return ret;
}

static inline uint findLeastCommonMultiple(uint a, uint b)
{
  uint lcm = (a > b) ? a : b;
  while (true) {
    if (lcm % a == 0 && lcm % b == 0) return lcm;
    lcm++;
  }
}

static inline uint findGreatestPrimeFactor(uint a)
{
  if (a == 0) return 0;
  else if (a == 1) return 1;

  // Every time it hits a factor, it will divide the number by it
  // It will end on the greatest prime factor
  for (size_t i = 2; i < a; i++) {
    if (a % i == 0) {
      a /= i;
      i = 2;
    }
  }
  return a;
}

// This is a recursive function
void buildSupercell(XcMatrix& cell, std::vector<uint>& types,
                    std::vector<XcVector>& pos, uint FU, uint targetFU)
{
  assert(targetFU % FU == 0);

  // Find the largest prime number multiple. We will expand
  // upon the shortest length of the cell with this number. We will perform
  // the other duplications through recursion of this whole function.
  uint totalNumDuplications = targetFU / FU;
  uint numDuplications = findGreatestPrimeFactor(totalNumDuplications);

  // Find the shortest side
  // 0 == A, 1 == B, 2 == C
  double A = cell.row(0).norm();
  double B = cell.row(1).norm();
  double C = cell.row(2).norm();

  uint shortestSide = 0;
  if (B < A && B < C) shortestSide = 1;
  else if (C < A && C < B) shortestSide = 2;

  // Expand the cell
  for (size_t i = 0; i <= 2; i++) cell(shortestSide, i) *= numDuplications;

#ifdef SUPERCELL_BUILDER_DEBUG
  std::cerr << "A is " << A << ", B is " << B << ", and C is " << C << "\n";
  std::cerr << "After " << numDuplications << " duplications on vector number " << shortestSide << ":\n";
  std::cerr << "A is now " << cell.row(0).norm() << ", B is " << cell.row(1).norm() << ", and C is " << cell.row(2).norm() << "\n";
#endif

  // Create a new types vector with the correct types
  std::vector<uint> newTypes;
  for (size_t i = 0; i < types.size(); i++) {
    for (size_t j = 0; j < numDuplications; j++) {
      newTypes.push_back(types.at(i));
    }
  }
  types = newTypes;

#ifdef SUPERCELL_BUILDER_DEBUG
  std::cerr << "Old atom positions were as follows:\n";
  for (size_t i = 0; i < pos.size(); i++) {
    for (size_t j = 0; j <= 2; j++) {
      std::cerr << pos.at(i)(j) << " ";
    }
    std::cerr << "\n";
  }
#endif

  // Create a new atom vector with the correct atoms in it
  std::vector<XcVector> newPos;
  for (size_t i = 0; i < pos.size(); i++) {
    for (size_t j = 0; j < numDuplications; j++) {
      XcVector tmp(pos.at(i));
      tmp(shortestSide) /= numDuplications;
      double lenOfOneUnit = 1.0 / static_cast<double>(numDuplications);
      tmp(shortestSide) += (static_cast<double>(j) * lenOfOneUnit);
      newPos.push_back(tmp);
    }
  }
  pos = newPos;

#ifdef SUPERCELL_BUILDER_DEBUG
  std::cerr << "New atom positions are as follows:\n";
  for (size_t i = 0; i < pos.size(); i++) {
    for (size_t j = 0; j <= 2; j++) {
      std::cerr << pos.at(i)(j) << " ";
    }
    std::cerr << "\n";
  }
#endif

  // We have a new FU
  uint newFU = getFormulaUnits(types);

  // If we aren't done, call the function again
  if (newFU != targetFU) buildSupercell(cell, types, pos, newFU, targetFU);
}

// XtalComp CGI wrapper:
int main() {
  char ** cgivars = getcgivars();

  XcMatrix cell1 (4.5, 0.0, 0.0,
                  1.2, 2.4, 0.0,
                  2.5, 6.4, 1.1);

  XcMatrix cell2 (2.5, 0.0, 0.0,
                  2.3, 4.2, 0.0,
                  1.5, 2.1, 2.1);

  std::vector<XcVector> pos1 (5);
  std::vector<XcVector> pos2 (5);

  std::vector<unsigned int> types1 (5);
  std::vector<unsigned int> types2 (5);

  float cartTol, angleTol;

  float transform[16];

  bool validInput = true;

  for (int i=0; cgivars[i]; i+= 2) {
    char *key = cgivars[i];
    char *val = cgivars[i+1];
    if (strcmp(key, "pos1") == 0) {
      if (!parsePOSCAR(val, cell1, pos1, types1)) validInput = false;
    }
    else if (strcmp(key, "pos2") == 0) {
      if (!parsePOSCAR(val, cell2, pos2, types2)) validInput = false;
    }
    else if (strcmp(key, "cartTol") == 0) {
      if (sscanf(val, "%f", &cartTol) != 1) validInput = false;
    }
    else if (strcmp(key, "angleTol") == 0) {
      if (sscanf(val, "%f", &angleTol) != 1) validInput = false;
    }
  }

  if (!validInput) {
    printf("Content-type: text/html\n\n");
    printf("<html>\n");
    printf("<head><title>XtalComp Results</title></head>\n");
    printf("<body>\n");
    printf("<h1>Invalid input</h1>\n");
    printf("Go back and check your inputs.\n");
    printf("<br>");
    //printf(debug.c_str());
    printf("</body>\n");
    printf("</html>\n");
    return 1;
  }

  bool supercellGenerated1 = false, supercellGenerated2 = false;

  // If the counts are not equal, see if we should build a supercell to
  // compare them
  if (types1.size() != types2.size()) {
    uint FU1 = getFormulaUnits(types1);
    uint FU2 = getFormulaUnits(types2);
    std::vector<uint> numOfEachType1 = countNumOfEachType<uint>(types1);
    std::vector<uint> numOfEachType2 = countNumOfEachType<uint>(types2);

    std::vector<uint> empiricalFormulaCounts1;
    std::vector<uint> empiricalFormulaCounts2;

    for (size_t i = 0; i < numOfEachType1.size(); i++)
      empiricalFormulaCounts1.push_back(numOfEachType1.at(i) / FU1);
    for (size_t i = 0; i < numOfEachType2.size(); i++)
      empiricalFormulaCounts2.push_back(numOfEachType2.at(i) / FU2);

    // If these two are equal, they have the same empirical formula and we
    // can build supercells to compare them!
    if (empiricalFormulaCounts1 == empiricalFormulaCounts2) {
      // We are going to build supercells so that each cell has an FU equal
      // to the least common multiple (LCM) of the two
      uint lcm = findLeastCommonMultiple(FU1, FU2);

      if (lcm != FU1) {
        buildSupercell(cell1, types1, pos1, FU1, lcm);
        supercellGenerated1 = true;
      }
      if (lcm != FU2) {
        buildSupercell(cell2, types2, pos2, FU2, lcm);
        supercellGenerated2 = true;
      }
    }
  }

  bool match = XtalComp::compare(cell1, types1, pos1,
                                 cell2, types2, pos2,
                                 transform, cartTol, angleTol);

  printf("Content-type: text/html\n\n");

  printf("<html>\n");
  printf("<head><title>XtalComp Results</title></head>\n");
  printf("<body>\n");
  printf("<h1>Result:</h1>\n");
  printf("Using a cartesian tolerance of %f and an angular tolerance of %f...<br><br>\n",
         cartTol, angleTol);
  printf("The structures %s match!<br>\n", (match) ? "DO" : "do NOT");
  if (match) { // Print transform
    printf("<font face=\"Courier New, Courier, monospace\">\n");
    printf("<pre>\n");
    printf("<h2>Transformation matrix:</h2>\n");
    printf("|--%10s--%10s--%10s--%10s--|\n", "----------", "----------",
           "----------", "----------");
    printf("|  %+10.5f  %+10.5f  %+10.5f  %+10.5f  |\n",
           transform[0*4+0], transform[0*4+1], transform[0*4+2], transform[0*4+3]);
    printf("|  %+10.5f  %+10.5f  %+10.5f  %+10.5f  |\n",
           transform[1*4+0], transform[1*4+1], transform[1*4+2], transform[1*4+3]);
    printf("|  %+10.5f  %+10.5f  %+10.5f  %+10.5f  |\n",
           transform[2*4+0], transform[2*4+1], transform[2*4+2], transform[2*4+3]);
    printf("|  %+10.5f  %+10.5f  %+10.5f  %+10.5f  |\n",
           transform[3*4+0], transform[3*4+1], transform[3*4+2], transform[3*4+3]);
    printf("|--%10s--%10s--%10s--%10s--|\n", "----------", "----------",
           "----------", "----------");
    printf("</pre>\n");
    printf("</font>\n") ;
    }

  printf("<h1>Input structures:</h1>\n");

  if (supercellGenerated1)
    printf("NOTE: a supercell was generated for cell1 because the two cells did not have the same numbers of atoms<br>\n");
  if (supercellGenerated2)
    printf("NOTE: a supercell was generated for cell2 because the two cells did not have the same numbers of atoms<br>\n");

  printf("<font face=\"Courier New, Courier, monospace\">\n");
  printf("<pre>\n");
  PRINT_DIV;
  printf("| %-35s | %-35s |\n",
         "First cell matrix (row vectors)",
         "Second cell matrix (row vectors)");
  PRINT_DIV;
  printf("| %9.5f %9.5f %9.5f %5s | %9.5f %9.5f %9.5f %5s |\n",
         cell1[0][0], cell1[0][1], cell1[0][2], "",
         cell2[0][0], cell2[0][1], cell2[0][2], "");
  printf("| %9.5f %9.5f %9.5f %5s | %9.5f %9.5f %9.5f %5s |\n",
         cell1[1][0], cell1[1][1], cell1[1][2], "",
         cell2[1][0], cell2[1][1], cell2[1][2], "");
  printf("| %9.5f %9.5f %9.5f %5s | %9.5f %9.5f %9.5f %5s |\n",
         cell1[2][0], cell1[2][1], cell1[2][2], "",
         cell2[2][0], cell2[2][1], cell2[2][2], "");
  PRINT_DIV;
  printf("| %-35s | %-35s |\n",
         "type: fractional coordinate",
         "type: fractional coordinate");
  PRINT_DIV;
  for (int i = 0; i < types1.size(); ++i) {
    printf("| %3hu: %9.5f %9.5f %9.5f %0s | %3hu: %9.5f %9.5f %9.5f %0s |\n",
           types1[i], pos1[i][0], pos1[i][1], pos1[i][2], "",
           types2[i], pos2[i][0], pos2[i][1], pos2[i][2], "");
  }
  PRINT_DIV;

  //printf("debug: \n%s\n", debug.c_str());
  printf("</pre>\n");
  printf("</font>\n") ;
  //printf("<!--#include virtual=\"xtalcompcounter.cgi\" -->\n");
  printf("Comparisons performed since June 6th, 2011: "
         "<br><embed src=\"xtalcompcounter.cgi\" />\n");
  printf("</body>\n") ;
  printf("</html>\n") ;

  /** Free anything that needs to be freed **/
  for (int i=0; cgivars[i]; i++) free(cgivars[i]) ;
  free(cgivars) ;

  exit(0) ;
}

void Debug(const char *str, const double d)
{
  char buffer[128];
  snprintf(buffer, 32, "%s %f\n", str, d);
  debug += buffer;
}
void Debug(const std::string &str, const double d) {Debug(str.c_str(), d);}

// A simple function to remove leading and trailing white space
void trim(std::string& s)
{
  size_t p = s.find_first_not_of(" \t");
  s.erase(0, p);

  p = s.find_last_not_of(" \t");
  if (std::string::npos != p) s.erase(p+1);
}

// A simple function that checks to see if a string only contains ints
// and spaces
inline bool isIntLine(const std::string& s)
{
  for (size_t i = 0; i < s.size(); i++) {
    // If it's not a digit, a space, or a return, return false
    if (!isdigit(s.at(i)) && s.at(i) != ' ' && s.at(i) != '\r') return false;
  }
  return true;
}

bool parsePOSCAR(char *str, XcMatrix &cell,
                 std::vector<XcVector> &pos,
                 std::vector<unsigned int> &types)
{
  std::string stdstr (str);
  std::istringstream lines (stdstr);
  std::string line;
  bool cart = false;

  // First line is comment
  getline(lines, line);

  // Next line is scale factor
  getline(lines, line);
  float scale;
  if (sscanf(line.c_str(), "%f", &scale) != 1) return false;

  // Next comes the matrix
  float x,y,z;
  getline(lines, line);
  if (sscanf(line.c_str(), "%f %f %f",
             &x, &y, &z) != 3) return false;
  cell[0][0] = x;
  cell[0][1] = y;
  cell[0][2] = z;
  getline(lines, line);
  if (sscanf(line.c_str(), "%f %f %f",
             &x, &y, &z) != 3) return false;
  cell[1][0] = x;
  cell[1][1] = y;
  cell[1][2] = z;
  getline(lines, line);
  if (sscanf(line.c_str(), "%f %f %f",
             &x, &y, &z) != 3) return false;
  cell[2][0] = x;
  cell[2][1] = y;
  cell[2][2] = z;

  // Apply scale:
  cell *= scale;

  // Store frac->cart matrix
  XcMatrix toCart = cell.transpose().inverse();

  // Sometimes, atomic symbols go here.
  // If we have something here that is not an int, ignore it and move to the
  // next line. Trim it first.
  getline(lines, line);
  trim(line);
  // If it's not an int, move on to the next line
  if (!isIntLine(line)) getline(lines, line);

  // List of atom types
  std::vector<int> counts (15); // Allow up to 15 atom types.
  int tmpint;
  int numTypes = sscanf(line.c_str(), "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
                        &counts[0], &counts[1], &counts[2],
                        &counts[3], &counts[4], &counts[5],
                        &counts[6], &counts[7], &counts[8],
                        &counts[9], &counts[10], &counts[11],
                        &counts[12], &counts[13], &counts[14], &tmpint);

  if (numTypes > 15) return false;

  // Starts with either [Ss]elective dynamics, [KkCc]artesian, or
  // other for fractional coords.
  getline(lines, line);

  // If selective dynamics, get the next line
  if (line.at(0) == 'S' || line.at(0) == 's')
    getline(lines, line);

  // Check if we're using cartesian or fractional coordinates:
  if (line.at(0) == 'K' || line.at(0) == 'k' ||
      line.at(0) == 'C' || line.at(0) == 'c' )
    cart = true;
  else
    cart = false;


  // Coordinates
  // determine number of atoms:
  types.clear();
  int numAtoms = 0;
  for (int i = 0; i < numTypes; ++i) {
    numAtoms += counts[i];
    for (int j = 0; j < counts[i]; ++j) {
      types.push_back(i);
    }
  }

  types.resize(numAtoms);

  Debug("numAtoms:", numAtoms);

  // Grab vectors
  XcVector tmp;
  pos.clear();
  for (int atom_i = 0; atom_i < numAtoms; ++atom_i) {
    getline(lines, line);
    if (sscanf(line.c_str(), "%f %f %f",
               &x, &y, &z) != 3) return false;
    tmp = XcVector(x,y,z);
    debug += "pos line: " + line + "\n";
    Debug("x: ", tmp.x());
    Debug("y: ", tmp.y());
    Debug("z: ", tmp.z());
    if (cart) {
      tmp = toCart * tmp;
      debug += "Converted to cartesian:\n";
      Debug("x: ", tmp.x());
      Debug("y: ", tmp.y());
      Debug("z: ", tmp.z());
    }
    pos.push_back(tmp);
  }

  Debug("pos size: ", pos.size());
  Debug("types size: ", types.size());
  return true;
}
