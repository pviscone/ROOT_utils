// root_diff.cpp - Complete Working Version
// Compile with: g++ -o root_diff root_diff.cpp `root-config --cflags --libs`
// -std=c++17

#include <algorithm>
#include <cmath>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <map>
#include <memory>
#include <regex>
#include <set>
#include <sstream>
#include <string>
#include <vector>

#include "TBranch.h"
#include "TFile.h"
#include "TLeaf.h"
#include "TTree.h"
#include "TTreeReader.h"
#include "TTreeReaderArray.h"
#include "TTreeReaderValue.h"

#include <sys/ioctl.h>
#include <unistd.h>

namespace Color {
const std::string RESET = "\033[0m";
const std::string BOLD = "\033[1m";
const std::string DIM = "\033[2m";
const std::string RED = "\033[31m";
const std::string GREEN = "\033[32m";
const std::string YELLOW = "\033[33m";
const std::string BLUE = "\033[34m";
const std::string MAGENTA = "\033[35m";
const std::string CYAN = "\033[36m";
const std::string BOLD_RED = "\033[1;31m";
const std::string BOLD_GREEN = "\033[1;32m";
const std::string BOLD_YELLOW = "\033[1;33m";
const std::string BOLD_BLUE = "\033[1;34m";
const std::string BOLD_MAGENTA = "\033[1;35m";
const std::string BOLD_CYAN = "\033[1;36m";
} // namespace Color

bool DEBUG = false;
bool COMPACT = false;
double ABS_TOLERANCE = -1.0;
double REL_TOLERANCE = -1.0;

void debugPrint(const std::string &message) {
  if (DEBUG) {
    std::cerr << Color::BOLD_YELLOW << "[DEBUG] " << Color::RESET << message
              << std::endl;
  }
}

// Helper function to repeat a UTF-8 string n times
std::string repeatString(const std::string &str, size_t n) {
  std::string result;
  result.reserve(str.length() * n);
  for (size_t i = 0; i < n; i++) {
    result += str;
  }
  return result;
}

// Create horizontal line with box-drawing character
std::string horizontalLine(size_t length) { return repeatString("─", length); }

// Create equals line for event header
std::string equalsLine(size_t length) { return repeatString("═", length); }

int getTerminalWidth() {
  struct winsize w;
  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &w) != -1) {
    if (w.ws_col > 0 && w.ws_col < 1000)
      return w.ws_col;
  }
  return 80;
}

std::string createEventHeader(Long64_t eventIdx) {
  int termWidth = getTerminalWidth();
  std::string eventText = " Event " + std::to_string(eventIdx) + " ";
  int textLen = eventText.length();

  int remaining = std::max(0, termWidth - textLen - 2);
  if (remaining > 500)
    remaining = 500;

  int leftPad = remaining / 2;
  int rightPad = remaining - leftPad;

  std::string header;
  try {
    header = Color::BOLD_MAGENTA + "╔" + equalsLine(leftPad) + // FIXED
             eventText + equalsLine(rightPad) +                // FIXED
             "╗" + Color::RESET;
  } catch (const std::exception &e) {
    debugPrint("Error creating event header: " + std::string(e.what()));
    header = Color::BOLD_MAGENTA + "╔═══ Event " + std::to_string(eventIdx) +
             " ═══╗" + Color::RESET;
  }

  return header;
}

std::string createCollectionHeader(const std::string &collectionName) {
  return Color::BOLD_CYAN + "──── " + collectionName + " ────" + Color::RESET;
}

std::string getBaseName(const std::string &path) {
  size_t slash = path.find_last_of("/");
  if (slash == std::string::npos)
    return path;
  return path.substr(slash + 1);
}

std::string createSectionHeader(const std::string &title) {
  return Color::BOLD_MAGENTA + "═══ " + title + " ═══" + Color::RESET;
}

size_t getVisibleLength(const std::string &str);
std::string extractCollectionName(const std::string &branchName);
std::string buildInfoBox(const std::vector<std::string> &lines);
void printPanelsInColumns(const std::vector<std::string> &panels);

void printInfoBox(const std::vector<std::string> &lines) {
  if (lines.empty())
    return;
  std::string box = buildInfoBox(lines);
  std::cout << box << "\n\n";
}

std::string buildInfoBox(const std::vector<std::string> &lines) {
  if (lines.empty())
    return "";

  size_t maxLen = 0;
  for (const auto &line : lines) {
    maxLen = std::max(maxLen, getVisibleLength(line));
  }

  size_t boxWidth = maxLen + 4; // borders + padding

  std::ostringstream out;
  out << Color::CYAN << "╭" << horizontalLine(boxWidth - 2) << "╮"
      << Color::RESET << "\n";
  for (const auto &line : lines) {
    size_t pad = boxWidth - 4 - getVisibleLength(line);
    out << Color::CYAN << "│ " << Color::RESET << line << std::string(pad, ' ')
        << Color::CYAN << " │" << Color::RESET << "\n";
  }
  out << Color::CYAN << "╰" << horizontalLine(boxWidth - 2) << "╯"
      << Color::RESET;

  return out.str();
}

std::map<std::string, std::vector<std::string>>
groupByCollection(const std::set<std::string> &branches) {
  std::map<std::string, std::vector<std::string>> grouped;
  for (const auto &b : branches) {
    grouped[extractCollectionName(b)].push_back(b);
  }
  return grouped;
}

void printCollectionBoxes(
    const std::map<std::string, std::vector<std::string>> &grouped,
    const std::string &marker) {
  if (grouped.empty()) {
    std::vector<std::string> lines{Color::BOLD_YELLOW + std::string("(none)") +
                                   Color::RESET};
    printInfoBox(lines);
    return;
  }

  std::vector<std::string> panels;
  for (const auto &[collection, branches] : grouped) {
    std::vector<std::string> lines;
    lines.push_back(Color::BOLD_CYAN + collection + Color::RESET);
    for (const auto &b : branches) {
      lines.push_back("  " + marker + " " + b);
    }
    panels.push_back(buildInfoBox(lines));
  }

  printPanelsInColumns(panels);
  std::cout << "\n";
}

std::string extractCollectionName(const std::string &branchName) {
  size_t pos = branchName.find('_');
  if (pos != std::string::npos) {
    return branchName.substr(0, pos);
  }
  return "Other";
}

bool matchesPatterns(const std::string &branchName,
                     const std::vector<std::regex> &patterns) {
  for (const auto &pattern : patterns) {
    if (std::regex_search(branchName, pattern)) {
      return true;
    }
  }
  return false;
}

std::vector<std::regex> compileBranchPatterns(const std::string &patternsStr) {
  std::vector<std::regex> patterns;
  std::stringstream ss(patternsStr);
  std::string pattern;

  while (std::getline(ss, pattern, ',')) {
    pattern.erase(0, pattern.find_first_not_of(" \t"));
    pattern.erase(pattern.find_last_not_of(" \t") + 1);
    if (!pattern.empty()) {
      try {
        patterns.push_back(std::regex(pattern));
      } catch (const std::regex_error &e) {
        std::cerr << Color::BOLD_RED << "Error: " << Color::RESET
                  << "Invalid regex pattern: " << pattern << std::endl;
      }
    }
  }
  return patterns;
}

template <typename T> bool compareValues(T val1, T val2) {
  if constexpr (std::is_floating_point_v<T>) {
    if (std::isnan(val1) && std::isnan(val2))
      return true;
    if (ABS_TOLERANCE >= 0)
      return std::abs(val1 - val2) <= ABS_TOLERANCE;
    if (REL_TOLERANCE >= 0)
      return std::abs(val1 - val2) <= REL_TOLERANCE * std::abs(val2);
    return std::abs(val1 - val2) < 1e-9;
  }
  return val1 == val2;
}

template <typename T1, typename T2> bool compareMixedValues(T1 val1, T2 val2) {
  if constexpr (std::is_floating_point_v<T1> || std::is_floating_point_v<T2>) {
    long double d1 = static_cast<long double>(val1);
    long double d2 = static_cast<long double>(val2);
    if (std::isnan(d1) && std::isnan(d2))
      return true;
    if (ABS_TOLERANCE >= 0)
      return std::abs(d1 - d2) <= ABS_TOLERANCE;
    if (REL_TOLERANCE >= 0)
      return std::abs(d1 - d2) <= REL_TOLERANCE * std::abs(d2);
    return std::abs(d1 - d2) < 1e-9;
  }
  return static_cast<long double>(val1) == static_cast<long double>(val2);
}

template <typename T> std::string formatValue(T val) {
  if constexpr (std::is_floating_point_v<T>) {
    if (std::isnan(val))
      return "*";
    std::ostringstream oss;
    oss << std::setprecision(6) << val;
    std::string result = oss.str();
    if (result.find('.') != std::string::npos) {
      result.erase(result.find_last_not_of('0') + 1);
      if (result.back() == '.')
        result.pop_back();
    }
    return result;
  } else if constexpr (std::is_same_v<T, bool>) {
    return val ? "True" : "False";
  } else {
    return std::to_string(val);
  }
}

struct DiffEntry {
  int index;
  std::string val1Str;
  std::string val2Str;
  bool isDifferent;
  bool isFloat1;
  bool isFloat2;
};

size_t getVisibleLength(const std::string &str) {
  size_t len = 0;
  bool inAnsi = false;

  for (size_t i = 0; i < str.length();) {
    unsigned char c = str[i];

    if (c == '\033') {
      // Start of ANSI sequence
      inAnsi = true;
      i++;
    } else if (inAnsi && c == 'm') {
      // End of ANSI sequence
      inAnsi = false;
      i++;
    } else if (inAnsi) {
      // Inside ANSI sequence, skip
      i++;
    } else if ((c & 0x80) == 0) {
      // ASCII character (1 byte)
      len++;
      i++;
    } else if ((c & 0xE0) == 0xC0) {
      // 2-byte UTF-8 character
      len++;
      i += 2;
    } else if ((c & 0xF0) == 0xE0) {
      // 3-byte UTF-8 character (box drawing chars are here)
      len++;
      i += 3;
    } else if ((c & 0xF8) == 0xF0) {
      // 4-byte UTF-8 character
      len++;
      i += 4;
    } else {
      // Invalid UTF-8, skip
      i++;
    }
  }

  return len;
}

std::vector<std::string>
alignNumericStrings(const std::vector<std::string> &strings,
                    const std::vector<bool> &isFloats) {
  if (strings.empty())
    return strings;

  std::vector<std::string> result;
  bool hasFloats = false;
  for (bool f : isFloats)
    if (f) {
      hasFloats = true;
      break;
    }

  if (!hasFloats) {
    size_t maxLen = 0;
    for (const auto &s : strings)
      maxLen = std::max(maxLen, s.length());
    for (const auto &s : strings)
      result.push_back(std::string(maxLen - s.length(), ' ') + s);
    return result;
  }

  int maxInt = 0, maxDec = 0;
  for (const auto &s : strings) {
    if (s == "*")
      continue;
    size_t dotPos = s.find('.');
    if (dotPos != std::string::npos) {
      maxInt = std::max(maxInt, (int)dotPos);
      maxDec = std::max(maxDec, (int)(s.length() - dotPos - 1));
    } else if (s.find('e') == std::string::npos) {
      maxInt = std::max(maxInt, (int)s.length());
    }
  }

  for (const auto &s : strings) {
    if (s == "*") {
      int width = maxInt + (maxDec > 0 ? 1 + maxDec : 0);
      result.push_back(std::string(std::max(0, width - 1), ' ') + "*");
    } else if (s.find('e') != std::string::npos) {
      int width = maxInt + (maxDec > 0 ? 1 + maxDec : 0);
      result.push_back(std::string(std::max(0, width - (int)s.length()), ' ') +
                       s);
    } else {
      size_t dotPos = s.find('.');
      if (dotPos != std::string::npos) {
        std::string intPart = s.substr(0, dotPos);
        std::string decPart = s.substr(dotPos + 1);
        result.push_back(std::string(maxInt - intPart.length(), ' ') + intPart +
                         "." + decPart +
                         std::string(maxDec - decPart.length(), ' '));
      } else {
        result.push_back(std::string(maxInt - s.length(), ' ') + s +
                         (maxDec > 0 ? std::string(1 + maxDec, ' ') : ""));
      }
    }
  }
  return result;
}

std::string createBranchPanel(const std::string &branchName,
                              const std::vector<DiffEntry> &entries,
                              bool compact) {
  std::vector<DiffEntry> filteredEntries;
  if (compact) {
    for (const auto &e : entries)
      if (e.isDifferent)
        filteredEntries.push_back(e);
  } else {
    filteredEntries = entries;
  }
  if (filteredEntries.empty())
    return "";

  std::vector<std::string> val1Strs, val2Strs;
  std::vector<bool> val1IsFloats, val2IsFloats;
  for (const auto &e : filteredEntries) {
    val1Strs.push_back(e.val1Str);
    val2Strs.push_back(e.val2Str);
    val1IsFloats.push_back(e.isFloat1);
    val2IsFloats.push_back(e.isFloat2);
  }

  auto alignedVal1 = alignNumericStrings(val1Strs, val1IsFloats);
  auto alignedVal2 = alignNumericStrings(val2Strs, val2IsFloats);

  int maxIdxWidth = compact && !filteredEntries.empty()
                        ? std::to_string(filteredEntries.back().index).length()
                        : 1;

  // Calculate the EXACT width each line will have
  size_t idxWidth = 0;
  if (compact) {
    idxWidth = maxIdxWidth + 3; // [idx] + space
  }

  // Arrow width is always the same
  size_t arrowWidth = 6; // "  ->  "

  // Value widths - these are already aligned, so all val1 have same length, all
  // val2 have same length
  size_t val1Width = alignedVal1.empty() ? 0 : alignedVal1[0].length();
  size_t val2Width = alignedVal2.empty() ? 0 : alignedVal2[0].length();

  // Total content width is fixed for all lines
  size_t contentWidth = idxWidth + val1Width + arrowWidth + val2Width;

  // Panel width must fit both title and content
  size_t titleWidth = branchName.length();
  size_t panelContentWidth = std::max(contentWidth, titleWidth);

  // Build colored lines - all will be exactly panelContentWidth
  std::vector<std::string> coloredLines;
  for (size_t i = 0; i < filteredEntries.size(); i++) {
    const auto &e = filteredEntries[i];
    std::ostringstream ls;
    size_t lineLen = 0;

    if (compact) {
      std::string idx =
          "[" +
          std::string(maxIdxWidth - std::to_string(e.index).length(), ' ') +
          std::to_string(e.index) + "]";
      ls << Color::DIM << Color::CYAN << idx << Color::RESET << " ";
      lineLen += idx.length() + 1;
    }

    if (alignedVal1[i].find('*') != std::string::npos)
      ls << Color::RED << alignedVal1[i] << Color::RESET;
    else if (e.isDifferent)
      ls << Color::YELLOW << alignedVal1[i] << Color::RESET;
    else
      ls << Color::DIM << alignedVal1[i] << Color::RESET;
    lineLen += alignedVal1[i].length();

    if (e.isDifferent) {
      ls << "  " << Color::DIM << Color::CYAN << "->" << Color::RESET << "  ";
    } else {
      ls << std::string(arrowWidth, ' ');
    }
    lineLen += arrowWidth;

    if (alignedVal2[i].find('*') != std::string::npos)
      ls << Color::RED << alignedVal2[i] << Color::RESET;
    else if (e.isDifferent)
      ls << Color::GREEN << alignedVal2[i] << Color::RESET;
    else
      ls << Color::DIM << alignedVal2[i] << Color::RESET;
    lineLen += alignedVal2[i].length();

    // Pad to panelContentWidth
    if (lineLen < panelContentWidth) {
      ls << std::string(panelContentWidth - lineLen, ' ');
    }

    coloredLines.push_back(ls.str());
  }

  std::ostringstream panel;

  // Top border with title centered
  size_t totalBorderWidth =
      panelContentWidth + 2; // +2 for spaces around content
  size_t leftPad = (totalBorderWidth - titleWidth) / 2;
  size_t rightPad = totalBorderWidth - titleWidth - leftPad;

  panel << Color::BLUE << "╭" << horizontalLine(leftPad) << Color::RESET
        << Color::BOLD << Color::BLUE << branchName << Color::RESET
        << Color::BLUE << horizontalLine(rightPad) << "╮" << Color::RESET
        << "\n";

  // Content lines with borders
  for (const auto &line : coloredLines) {
    panel << Color::BLUE << "│ " << Color::RESET << line << Color::BLUE << " │"
          << Color::RESET << "\n";
  }

  // Bottom border
  panel << Color::BLUE << "╰" << horizontalLine(totalBorderWidth) << "╯"
        << Color::RESET;

  return panel.str();
}

std::vector<std::string> splitPanelLines(const std::string &panel) {
  std::vector<std::string> lines;
  std::istringstream stream(panel);
  std::string line;
  while (std::getline(stream, line))
    lines.push_back(line);
  return lines;
}

void printPanelsInColumns(const std::vector<std::string> &panels) {
  if (panels.empty())
    return;

  std::vector<std::vector<std::string>> panelLines;
  for (const auto &p : panels) {
    auto lines = splitPanelLines(p);
    if (!lines.empty()) {
      panelLines.push_back(lines);
    }
  }

  if (panelLines.empty())
    return;

  int termWidth = getTerminalWidth();
  std::vector<size_t> currentRow;
  int currentWidth = 0;

  for (size_t i = 0; i < panelLines.size(); i++) {
    if (panelLines[i].empty())
      continue;

    int panelWidth = getVisibleLength(panelLines[i][0]);

    if (panelWidth <= 0 || panelWidth > 1000) {
      debugPrint("Warning: Invalid panel width " + std::to_string(panelWidth));
      continue;
    }

    if (currentRow.empty() || currentWidth + panelWidth + 2 <= termWidth) {
      currentRow.push_back(i);
      currentWidth += panelWidth + 2;
    } else {
      size_t maxLines = 0;
      for (size_t idx : currentRow) {
        if (idx < panelLines.size()) {
          maxLines = std::max(maxLines, panelLines[idx].size());
        }
      }

      for (size_t ln = 0; ln < maxLines; ln++) {
        for (size_t j = 0; j < currentRow.size(); j++) {
          size_t idx = currentRow[j];
          if (idx < panelLines.size() && ln < panelLines[idx].size()) {
            std::cout << panelLines[idx][ln];
          } else if (idx < panelLines.size() && !panelLines[idx].empty()) {
            int panelW = getVisibleLength(panelLines[idx][0]);
            if (panelW > 0 && panelW < 1000) {
              std::cout << std::string(panelW, ' ');
            }
          }
          if (j < currentRow.size() - 1) {
            std::cout << "  ";
          }
        }
        std::cout << "\n";
      }

      currentRow = {i};
      currentWidth = panelWidth + 2;
    }
  }

  if (!currentRow.empty()) {
    size_t maxLines = 0;
    for (size_t idx : currentRow) {
      if (idx < panelLines.size()) {
        maxLines = std::max(maxLines, panelLines[idx].size());
      }
    }

    for (size_t ln = 0; ln < maxLines; ln++) {
      for (size_t j = 0; j < currentRow.size(); j++) {
        size_t idx = currentRow[j];
        if (idx < panelLines.size() && ln < panelLines[idx].size()) {
          std::cout << panelLines[idx][ln];
        } else if (idx < panelLines.size() && !panelLines[idx].empty()) {
          int panelW = getVisibleLength(panelLines[idx][0]);
          if (panelW > 0 && panelW < 1000) {
            std::cout << std::string(panelW, ' ');
          }
        }
        if (j < currentRow.size() - 1) {
          std::cout << "  ";
        }
      }
      std::cout << "\n";
    }
  }
}

bool isCollection(TTree *tree, const std::string &branchName) {
  TBranch *branch = tree->GetBranch(branchName.c_str());
  if (!branch)
    return false;

  std::string className = branch->GetClassName();
  if (!className.empty() && (className.find("RVec") != std::string::npos ||
                             className.find("vector") != std::string::npos)) {
    return true;
  }

  TObjArray *leaves = branch->GetListOfLeaves();
  if (leaves && leaves->GetEntries() > 0) {
    TLeaf *leaf = dynamic_cast<TLeaf *>(leaves->At(0));
    if (leaf && leaf->GetLeafCount() != nullptr) {
      return true;
    }
    if (leaf && (leaf->GetLen() > 1 || leaf->GetLenStatic() > 1)) {
      return true;
    }
  }

  return false;
}

std::string getElementType(TTree *tree, const std::string &branchName) {
  TBranch *branch = tree->GetBranch(branchName.c_str());
  if (!branch)
    return "unknown";

  // Get the leaf to check its type
  TLeaf *leaf = branch->GetLeaf(branchName.c_str());
  if (!leaf) {
    TObjArray *leaves = branch->GetListOfLeaves();
    if (leaves && leaves->GetEntries() > 0) {
      leaf = dynamic_cast<TLeaf *>(leaves->At(0));
    }
  }

  if (leaf) {
    std::string typeName = leaf->GetTypeName();

    // Try to get the title which sometimes contains type info
    std::string title = leaf->GetTitle();

    debugPrint("  Leaf type: " + typeName + ", Title: " + title);

    // For arrays or scalars, trust the leaf-reported element type
    if (typeName == "Float_t" || typeName == "float")
      return "float";
    if (typeName == "Double_t" || typeName == "double")
      return "double";
    if (typeName == "Int_t" || typeName == "int")
      return "int";
    if (typeName == "Bool_t" || typeName == "bool")
      return "bool";
    if (typeName == "Long64_t" || typeName == "long long")
      return "long";
    if (typeName == "Short_t" || typeName == "short")
      return "short";
    if (typeName == "Char_t" || typeName == "char")
      return "char";
    if (typeName == "UChar_t" || typeName == "unsigned char")
      return "uchar";
    if (typeName == "UInt_t" || typeName == "unsigned int")
      return "uint";
    if (typeName == "UShort_t" || typeName == "unsigned short")
      return "ushort";
  }

  // Fallback to class name only if the leaf is missing or unknown
  std::string className = branch->GetClassName();
  if (!className.empty()) {
    if (className.find("float") != std::string::npos)
      return "float";
    if (className.find("double") != std::string::npos)
      return "double";
    if (className.find("int") != std::string::npos)
      return "int";
    if (className.find("bool") != std::string::npos)
      return "bool";
    if (className.find("long") != std::string::npos)
      return "long";
    if (className.find("short") != std::string::npos ||
        className.find("Short") != std::string::npos)
      return "short";
    if (className.find("char") != std::string::npos)
      return "char";
    if (className.find("UChar") != std::string::npos)
      return "uchar";
    if (className.find("UInt") != std::string::npos)
      return "uint";
    if (className.find("UShort") != std::string::npos)
      return "ushort";
  }

  return "unknown";
}

enum class ElemTypeId {
  Float,
  Double,
  Int,
  Bool,
  Long,
  Short,
  Char,
  UChar,
  UInt,
  UShort,
  Unknown
};

ElemTypeId parseElemTypeId(const std::string &typeName) {
  if (typeName == "float")
    return ElemTypeId::Float;
  if (typeName == "double")
    return ElemTypeId::Double;
  if (typeName == "int")
    return ElemTypeId::Int;
  if (typeName == "bool")
    return ElemTypeId::Bool;
  if (typeName == "long")
    return ElemTypeId::Long;
  if (typeName == "short")
    return ElemTypeId::Short;
  if (typeName == "char")
    return ElemTypeId::Char;
  if (typeName == "uchar")
    return ElemTypeId::UChar;
  if (typeName == "uint")
    return ElemTypeId::UInt;
  if (typeName == "ushort")
    return ElemTypeId::UShort;
  return ElemTypeId::Unknown;
}

class BranchDiffHandler {
public:
  virtual ~BranchDiffHandler() = default;
  virtual bool processEvent(std::vector<DiffEntry> &entries) = 0;
  virtual std::string getBranchName() const = 0;
};

template <typename T> class ReaderScalarHandler : public BranchDiffHandler {
  std::string branchName;
  TTreeReaderValue<T> reader1, reader2;

public:
  ReaderScalarHandler(const std::string &name, TTreeReader &r1, TTreeReader &r2)
      : branchName(name), reader1(r1, name.c_str()), reader2(r2, name.c_str()) {
  }

  bool processEvent(std::vector<DiffEntry> &entries) override {
    T val1 = *reader1, val2 = *reader2;
    bool isDiff = !compareValues(val1, val2);
    DiffEntry e{
        0,      formatValue(val1),           formatValue(val2),
        isDiff, std::is_floating_point_v<T>, std::is_floating_point_v<T>};
    entries.push_back(e);
    return isDiff;
  }
  std::string getBranchName() const override { return branchName; }
};

template <typename T> class ReaderArrayHandler : public BranchDiffHandler {
  std::string branchName;
  TTreeReaderArray<T> reader1, reader2;

public:
  ReaderArrayHandler(const std::string &name, TTreeReader &r1, TTreeReader &r2)
      : branchName(name), reader1(r1, name.c_str()), reader2(r2, name.c_str()) {
  }

  bool processEvent(std::vector<DiffEntry> &entries) override {
    bool hasDiff = false;
    size_t size1 = reader1.GetSize(), size2 = reader2.GetSize();
    size_t maxSize = std::max(size1, size2);

    for (size_t i = 0; i < maxSize; i++) {
      DiffEntry e;
      e.index = i;
      e.isFloat1 = e.isFloat2 = std::is_floating_point_v<T>;

      if (i < size1 && i < size2) {
        T v1 = reader1[i], v2 = reader2[i];
        e.val1Str = formatValue(v1);
        e.val2Str = formatValue(v2);
        e.isDifferent = !compareValues(v1, v2);
      } else if (i < size1) {
        e.val1Str = formatValue(reader1[i]);
        e.val2Str = "*";
        e.isDifferent = true;
      } else {
        e.val1Str = "*";
        e.val2Str = formatValue(reader2[i]);
        e.isDifferent = true;
      }
      entries.push_back(e);
      if (e.isDifferent)
        hasDiff = true;
    }
    return hasDiff;
  }
  std::string getBranchName() const override { return branchName; }
};

template <typename T1, typename T2>
class ReaderScalarHandlerMixed : public BranchDiffHandler {
  std::string branchName;
  TTreeReaderValue<T1> reader1;
  TTreeReaderValue<T2> reader2;

public:
  ReaderScalarHandlerMixed(const std::string &name, TTreeReader &r1,
                           TTreeReader &r2)
      : branchName(name), reader1(r1, name.c_str()), reader2(r2, name.c_str()) {
  }

  bool processEvent(std::vector<DiffEntry> &entries) override {
    T1 val1 = *reader1;
    T2 val2 = *reader2;
    bool isDiff = !compareMixedValues(val1, val2);
    DiffEntry e{0,
                formatValue(val1),
                formatValue(val2),
                isDiff,
                std::is_floating_point_v<T1>,
                std::is_floating_point_v<T2>};
    entries.push_back(e);
    return isDiff;
  }

  std::string getBranchName() const override { return branchName; }
};

template <typename T1, typename T2>
class ReaderArrayHandlerMixed : public BranchDiffHandler {
  std::string branchName;
  TTreeReaderArray<T1> reader1;
  TTreeReaderArray<T2> reader2;

public:
  ReaderArrayHandlerMixed(const std::string &name, TTreeReader &r1,
                          TTreeReader &r2)
      : branchName(name), reader1(r1, name.c_str()), reader2(r2, name.c_str()) {
  }

  bool processEvent(std::vector<DiffEntry> &entries) override {
    bool hasDiff = false;
    size_t size1 = reader1.GetSize();
    size_t size2 = reader2.GetSize();
    size_t maxSize = std::max(size1, size2);

    for (size_t i = 0; i < maxSize; i++) {
      DiffEntry e;
      e.index = i;
      e.isFloat1 = std::is_floating_point_v<T1>;
      e.isFloat2 = std::is_floating_point_v<T2>;

      if (i < size1 && i < size2) {
        T1 v1 = reader1[i];
        T2 v2 = reader2[i];
        e.val1Str = formatValue(v1);
        e.val2Str = formatValue(v2);
        e.isDifferent = !compareMixedValues(v1, v2);
      } else if (i < size1) {
        e.val1Str = formatValue(reader1[i]);
        e.val2Str = "*";
        e.isDifferent = true;
      } else {
        e.val1Str = "*";
        e.val2Str = formatValue(reader2[i]);
        e.isDifferent = true;
      }

      entries.push_back(e);
      if (e.isDifferent)
        hasDiff = true;
    }

    return hasDiff;
  }

  std::string getBranchName() const override { return branchName; }
};

template <typename T1>
std::unique_ptr<BranchDiffHandler>
makeArrayHandlerForType2(ElemTypeId type2, const std::string &branchName,
                         TTreeReader &r1, TTreeReader &r2) {
  switch (type2) {
  case ElemTypeId::Float:
    return std::make_unique<ReaderArrayHandlerMixed<T1, float>>(branchName, r1,
                                                                r2);
  case ElemTypeId::Double:
    return std::make_unique<ReaderArrayHandlerMixed<T1, double>>(branchName, r1,
                                                                 r2);
  case ElemTypeId::Int:
    return std::make_unique<ReaderArrayHandlerMixed<T1, int>>(branchName, r1,
                                                              r2);
  case ElemTypeId::Bool:
    return std::make_unique<ReaderArrayHandlerMixed<T1, bool>>(branchName, r1,
                                                               r2);
  case ElemTypeId::Long:
    return std::make_unique<ReaderArrayHandlerMixed<T1, Long64_t>>(branchName,
                                                                   r1, r2);
  case ElemTypeId::Short:
    return std::make_unique<ReaderArrayHandlerMixed<T1, Short_t>>(branchName,
                                                                  r1, r2);
  case ElemTypeId::Char:
    return std::make_unique<ReaderArrayHandlerMixed<T1, Char_t>>(branchName, r1,
                                                                 r2);
  case ElemTypeId::UChar:
    return std::make_unique<ReaderArrayHandlerMixed<T1, UChar_t>>(branchName,
                                                                  r1, r2);
  case ElemTypeId::UInt:
    return std::make_unique<ReaderArrayHandlerMixed<T1, UInt_t>>(branchName, r1,
                                                                 r2);
  case ElemTypeId::UShort:
    return std::make_unique<ReaderArrayHandlerMixed<T1, UShort_t>>(branchName,
                                                                   r1, r2);
  default:
    return nullptr;
  }
}

template <typename T1>
std::unique_ptr<BranchDiffHandler>
makeScalarHandlerForType2(ElemTypeId type2, const std::string &branchName,
                          TTreeReader &r1, TTreeReader &r2) {
  switch (type2) {
  case ElemTypeId::Float:
    return std::make_unique<ReaderScalarHandlerMixed<T1, float>>(branchName, r1,
                                                                 r2);
  case ElemTypeId::Double:
    return std::make_unique<ReaderScalarHandlerMixed<T1, double>>(branchName,
                                                                  r1, r2);
  case ElemTypeId::Int:
    return std::make_unique<ReaderScalarHandlerMixed<T1, int>>(branchName, r1,
                                                               r2);
  case ElemTypeId::Bool:
    return std::make_unique<ReaderScalarHandlerMixed<T1, bool>>(branchName, r1,
                                                                r2);
  case ElemTypeId::Long:
    return std::make_unique<ReaderScalarHandlerMixed<T1, Long64_t>>(branchName,
                                                                    r1, r2);
  case ElemTypeId::Short:
    return std::make_unique<ReaderScalarHandlerMixed<T1, Short_t>>(branchName,
                                                                   r1, r2);
  case ElemTypeId::Char:
    return std::make_unique<ReaderScalarHandlerMixed<T1, Char_t>>(branchName,
                                                                  r1, r2);
  case ElemTypeId::UChar:
    return std::make_unique<ReaderScalarHandlerMixed<T1, UChar_t>>(branchName,
                                                                   r1, r2);
  case ElemTypeId::UInt:
    return std::make_unique<ReaderScalarHandlerMixed<T1, UInt_t>>(branchName,
                                                                  r1, r2);
  case ElemTypeId::UShort:
    return std::make_unique<ReaderScalarHandlerMixed<T1, UShort_t>>(branchName,
                                                                    r1, r2);
  default:
    return nullptr;
  }
}

std::unique_ptr<BranchDiffHandler>
createReaderHandler(const std::string &branchName, TTreeReader &r1,
                    TTreeReader &r2, TTree *tree1, TTree *tree2) {
  TBranch *branch1 = tree1->GetBranch(branchName.c_str());
  TBranch *branch2 = tree2->GetBranch(branchName.c_str());
  if (!branch1 || !branch2)
    return nullptr;

  std::string elemType1 = getElementType(tree1, branchName);
  std::string elemType2 = getElementType(tree2, branchName);
  ElemTypeId typeId1 = parseElemTypeId(elemType1);
  ElemTypeId typeId2 = parseElemTypeId(elemType2);

  bool isColl1 = isCollection(tree1, branchName);
  bool isColl2 = isCollection(tree2, branchName);

  debugPrint("Handler: " + branchName + ", Type1: " + elemType1 + ", Type2: " +
             elemType2 + ", IsCollection1: " + std::to_string(isColl1) +
             ", IsCollection2: " + std::to_string(isColl2));

  if (typeId1 == ElemTypeId::Unknown || typeId2 == ElemTypeId::Unknown) {
    debugPrint("Warning: Unknown element type for " + branchName);
    return nullptr;
  }

  if (isColl1 != isColl2) {
    debugPrint("Warning: Collection mismatch for " + branchName);
    return nullptr;
  }

  if (isColl1) {
    switch (typeId1) {
    case ElemTypeId::Float:
      return makeArrayHandlerForType2<float>(typeId2, branchName, r1, r2);
    case ElemTypeId::Double:
      return makeArrayHandlerForType2<double>(typeId2, branchName, r1, r2);
    case ElemTypeId::Int:
      return makeArrayHandlerForType2<int>(typeId2, branchName, r1, r2);
    case ElemTypeId::Bool:
      return makeArrayHandlerForType2<bool>(typeId2, branchName, r1, r2);
    case ElemTypeId::Long:
      return makeArrayHandlerForType2<Long64_t>(typeId2, branchName, r1, r2);
    case ElemTypeId::Short:
      return makeArrayHandlerForType2<Short_t>(typeId2, branchName, r1, r2);
    case ElemTypeId::Char:
      return makeArrayHandlerForType2<Char_t>(typeId2, branchName, r1, r2);
    case ElemTypeId::UChar:
      return makeArrayHandlerForType2<UChar_t>(typeId2, branchName, r1, r2);
    case ElemTypeId::UInt:
      return makeArrayHandlerForType2<UInt_t>(typeId2, branchName, r1, r2);
    case ElemTypeId::UShort:
      return makeArrayHandlerForType2<UShort_t>(typeId2, branchName, r1, r2);
    default:
      return nullptr;
    }
  }

  switch (typeId1) {
  case ElemTypeId::Float:
    return makeScalarHandlerForType2<float>(typeId2, branchName, r1, r2);
  case ElemTypeId::Double:
    return makeScalarHandlerForType2<double>(typeId2, branchName, r1, r2);
  case ElemTypeId::Int:
    return makeScalarHandlerForType2<int>(typeId2, branchName, r1, r2);
  case ElemTypeId::Bool:
    return makeScalarHandlerForType2<bool>(typeId2, branchName, r1, r2);
  case ElemTypeId::Long:
    return makeScalarHandlerForType2<Long64_t>(typeId2, branchName, r1, r2);
  case ElemTypeId::Short:
    return makeScalarHandlerForType2<Short_t>(typeId2, branchName, r1, r2);
  case ElemTypeId::Char:
    return makeScalarHandlerForType2<Char_t>(typeId2, branchName, r1, r2);
  case ElemTypeId::UChar:
    return makeScalarHandlerForType2<UChar_t>(typeId2, branchName, r1, r2);
  case ElemTypeId::UInt:
    return makeScalarHandlerForType2<UInt_t>(typeId2, branchName, r1, r2);
  case ElemTypeId::UShort:
    return makeScalarHandlerForType2<UShort_t>(typeId2, branchName, r1, r2);
  default:
    return nullptr;
  }
}

void printUsage(const char *progName) {
  std::cout << "Usage: " << progName << " FILE1 FILE2 [OPTIONS]\n\n";
  std::cout << "Options:\n";
  std::cout << "  -t, --tree NAME            Tree name (default: Events)\n";
  std::cout << "  -b, --branches PATTERN     Comma-separated regex patterns "
               "(default: .*)\n";
  std::cout << "  -n, --num-events N         Number of events to compare "
               "(default: -1, all)\n";
  std::cout << "  -s, --start N              Start from event N (default: 0)\n";
  std::cout
      << "  --abs-tolerance TOL        Absolute tolerance for floating point\n";
  std::cout
      << "  --rel-tolerance TOL        Relative tolerance for floating point\n";
  std::cout << "  --compact                  Show only differing elements\n";
  std::cout << "  --debug                    Enable debug output\n";
  std::cout << "  -h, --help                 Show this help\n";
}

std::vector<std::string> createAlignedPanels(
    const std::vector<std::pair<std::string, std::vector<DiffEntry>>>
        &branchData,
    bool compact) {
  if (branchData.empty())
    return {};

  // First pass: collect all values for alignment
  std::vector<std::string> allVal1Strs, allVal2Strs;
  std::vector<bool> allVal1IsFloats, allVal2IsFloats;
  size_t maxIdxWidth = 0;

  for (const auto &[branchName, entries] : branchData) {
    std::vector<DiffEntry> filtered;
    if (compact) {
      for (const auto &e : entries)
        if (e.isDifferent)
          filtered.push_back(e);
    } else {
      filtered = entries;
    }

    for (const auto &e : filtered) {
      allVal1Strs.push_back(e.val1Str);
      allVal2Strs.push_back(e.val2Str);
      allVal1IsFloats.push_back(e.isFloat1);
      allVal2IsFloats.push_back(e.isFloat2);
      if (compact) {
        maxIdxWidth =
            std::max(maxIdxWidth, (size_t)std::to_string(e.index).length());
      }
    }
  }

  // Align all values at once
  auto alignedAllVal1 = alignNumericStrings(allVal1Strs, allVal1IsFloats);
  auto alignedAllVal2 = alignNumericStrings(allVal2Strs, allVal2IsFloats);

  // Get the width after alignment
  size_t maxVal1Width = alignedAllVal1.empty() ? 0 : alignedAllVal1[0].length();
  size_t maxVal2Width = alignedAllVal2.empty() ? 0 : alignedAllVal2[0].length();

  // Second pass: create panels with aligned values
  std::vector<std::string> panels;
  size_t valueIndex = 0; // Track position in aligned arrays

  for (const auto &[branchName, entries] : branchData) {
    std::vector<DiffEntry> filtered;
    if (compact) {
      for (const auto &e : entries)
        if (e.isDifferent)
          filtered.push_back(e);
    } else {
      filtered = entries;
    }

    if (filtered.empty())
      continue;

    // Build content with aligned values
    std::vector<std::string> coloredLines;

    size_t idxWidth = 0;
    if (compact && maxIdxWidth > 0) {
      idxWidth = maxIdxWidth + 3; // [idx] + space
    }

    size_t arrowWidth = 6; // "  ->  "
    size_t contentWidth = idxWidth + maxVal1Width + arrowWidth + maxVal2Width;
    size_t titleWidth = branchName.length();
    size_t panelContentWidth = std::max(contentWidth, titleWidth);

    for (const auto &e : filtered) {
      std::ostringstream ls;
      size_t lineLen = 0;

      if (compact) {
        std::string idx =
            "[" +
            std::string(maxIdxWidth - std::to_string(e.index).length(), ' ') +
            std::to_string(e.index) + "]";
        ls << Color::DIM << Color::CYAN << idx << Color::RESET << " ";
        lineLen += idx.length() + 1;
      }

      // Use the pre-aligned value
      std::string val1Aligned = alignedAllVal1[valueIndex];
      if (val1Aligned.find('*') != std::string::npos)
        ls << Color::RED << val1Aligned << Color::RESET;
      else if (e.isDifferent)
        ls << Color::YELLOW << val1Aligned << Color::RESET;
      else
        ls << Color::DIM << val1Aligned << Color::RESET;
      lineLen += maxVal1Width;

      if (e.isDifferent) {
        ls << "  " << Color::DIM << Color::CYAN << "->" << Color::RESET << "  ";
      } else {
        ls << std::string(arrowWidth, ' ');
      }
      lineLen += arrowWidth;

      // Use the pre-aligned value
      std::string val2Aligned = alignedAllVal2[valueIndex];
      if (val2Aligned.find('*') != std::string::npos)
        ls << Color::RED << val2Aligned << Color::RESET;
      else if (e.isDifferent)
        ls << Color::GREEN << val2Aligned << Color::RESET;
      else
        ls << Color::DIM << val2Aligned << Color::RESET;
      lineLen += maxVal2Width;

      valueIndex++; // Move to next value

      // Pad to panelContentWidth
      if (lineLen < panelContentWidth) {
        ls << std::string(panelContentWidth - lineLen, ' ');
      }

      coloredLines.push_back(ls.str());
    }

    // Build panel
    std::ostringstream panel;

    size_t totalBorderWidth = panelContentWidth + 2;
    size_t leftPad = (totalBorderWidth - titleWidth) / 2;
    size_t rightPad = totalBorderWidth - titleWidth - leftPad;

    panel << Color::BLUE << "╭" << horizontalLine(leftPad) << Color::RESET
          << Color::BOLD << Color::BLUE << branchName << Color::RESET
          << Color::BLUE << horizontalLine(rightPad) << "╮" << Color::RESET
          << "\n";

    for (const auto &line : coloredLines) {
      panel << Color::BLUE << "│ " << Color::RESET << line << Color::BLUE
            << " │" << Color::RESET << "\n";
    }

    panel << Color::BLUE << "╰" << horizontalLine(totalBorderWidth) << "╯"
          << Color::RESET;

    panels.push_back(panel.str());
  }

  return panels;
}

int main(int argc, char **argv) {
  if (argc < 3) {
    printUsage(argv[0]);
    return 1;
  }

  std::string file1Path = argv[1], file2Path = argv[2];
  std::string treeName = "Events", branchPattern = ".*";
  Long64_t numEvents = -1, startEvent = 0;

  for (int i = 3; i < argc; i++) {
    std::string arg = argv[i];
    if (arg == "-t" && i + 1 < argc)
      treeName = argv[++i];
    else if (arg == "-b" && i + 1 < argc)
      branchPattern = argv[++i];
    else if (arg == "-n" && i + 1 < argc)
      numEvents = std::stoll(argv[++i]);
    else if (arg == "-s" && i + 1 < argc)
      startEvent = std::stoll(argv[++i]);
    else if (arg == "--abs-tolerance" && i + 1 < argc)
      ABS_TOLERANCE = std::stod(argv[++i]);
    else if (arg == "--rel-tolerance" && i + 1 < argc)
      REL_TOLERANCE = std::stod(argv[++i]);
    else if (arg == "--compact")
      COMPACT = true;
    else if (arg == "--debug")
      DEBUG = true;
    else if (arg == "-h" || arg == "--help") {
      printUsage(argv[0]);
      return 0;
    }
  }

  if (ABS_TOLERANCE >= 0 && REL_TOLERANCE >= 0) {
    std::cerr << Color::BOLD_RED << "Error: Cannot use both tolerances\n"
              << Color::RESET;
    return 1;
  }

  TFile *f1 = TFile::Open(file1Path.c_str());
  TFile *f2 = TFile::Open(file2Path.c_str());
  if (!f1 || !f2 || f1->IsZombie() || f2->IsZombie()) {
    std::cerr << Color::BOLD_RED << "Error opening files\n" << Color::RESET;
    return 1;
  }

  TTree *t1 = dynamic_cast<TTree *>(f1->Get(treeName.c_str()));
  TTree *t2 = dynamic_cast<TTree *>(f2->Get(treeName.c_str()));
  if (!t1 || !t2) {
    std::cerr << Color::BOLD_RED << "Error: tree not found\n" << Color::RESET;
    return 1;
  }

  auto patterns = compileBranchPatterns(branchPattern);
  std::set<std::string> branches1, branches2, common;

  for (int i = 0; i < t1->GetListOfBranches()->GetEntries(); i++) {
    std::string n = t1->GetListOfBranches()->At(i)->GetName();
    if (matchesPatterns(n, patterns))
      branches1.insert(n);
  }
  for (int i = 0; i < t2->GetListOfBranches()->GetEntries(); i++) {
    std::string n = t2->GetListOfBranches()->At(i)->GetName();
    if (matchesPatterns(n, patterns))
      branches2.insert(n);
  }
  std::set_intersection(branches1.begin(), branches1.end(), branches2.begin(),
                        branches2.end(), std::inserter(common, common.begin()));

  std::set<std::string> missingInFile1;
  std::set<std::string> missingInFile2;
  std::set_difference(branches2.begin(), branches2.end(), branches1.begin(),
                      branches1.end(),
                      std::inserter(missingInFile1, missingInFile1.begin()));
  std::set_difference(branches1.begin(), branches1.end(), branches2.begin(),
                      branches2.end(),
                      std::inserter(missingInFile2, missingInFile2.begin()));

  std::vector<std::string> infoLines;
  infoLines.push_back(Color::BOLD_CYAN + "Comparing files:" + Color::RESET);
  infoLines.push_back("  File 1: " + getBaseName(file1Path));
  infoLines.push_back("  File 2: " + getBaseName(file2Path));
  infoLines.push_back("  Tree: " + treeName);
  infoLines.push_back(std::string("  Mode: ") +
                      (COMPACT ? "Compact differences" : "Full collection"));
  if (REL_TOLERANCE >= 0) {
    infoLines.push_back("  Relative tolerance: " + formatValue(REL_TOLERANCE));
  } else if (ABS_TOLERANCE >= 0) {
    infoLines.push_back("  Absolute tolerance: " + formatValue(ABS_TOLERANCE));
  }
  printInfoBox(infoLines);

  std::cout << createSectionHeader("Missing Branches") << "\n\n";
  std::cout << "Missing in " << getBaseName(file1Path) << ":\n";
  if (missingInFile1.empty()) {
    std::cout << "  " << Color::BOLD_GREEN << "✓ None" << Color::RESET << "\n";
  } else {
    printCollectionBoxes(groupByCollection(missingInFile1),
                         Color::RED + std::string("✗") + Color::RESET);
  }
  std::cout << "\nMissing in " << getBaseName(file2Path) << ":\n";
  if (missingInFile2.empty()) {
    std::cout << "  " << Color::BOLD_GREEN << "✓ None" << Color::RESET << "\n";
  } else {
    printCollectionBoxes(groupByCollection(missingInFile2),
                         Color::RED + std::string("✗") + Color::RESET);
  }

  std::cout << "\n" << createSectionHeader("Inspected Branches") << "\n\n";
  std::cout << "Comparing " << common.size() << " branch(es):\n";
  if (common.empty()) {
    std::cout << "  " << Color::BOLD_YELLOW << "(none)" << Color::RESET << "\n";
  } else {
    printCollectionBoxes(groupByCollection(common),
                         Color::BOLD_GREEN + std::string("✓") + Color::RESET);
  }
  std::cout << "\n";

  TTreeReader r1(t1), r2(t2);
  std::map<std::string, std::unique_ptr<BranchDiffHandler>> handlers;
  for (const auto &b : common) {
    auto h = createReaderHandler(b, r1, r2, t1, t2);
    if (h)
      handlers[b] = std::move(h);
  }

  Long64_t maxEv = std::min(t1->GetEntries(), t2->GetEntries());
  Long64_t endEv =
      (numEvents == -1) ? maxEv : std::min(startEvent + numEvents, maxEv);

  std::map<Long64_t, std::map<std::string, std::vector<DiffEntry>>> evDiffs;

  for (Long64_t e = startEvent; e < endEv; e++) {
    r1.SetEntry(e);
    r2.SetEntry(e);

    for (auto &[bn, h] : handlers) {
      std::vector<DiffEntry> ents;
      if (h->processEvent(ents)) {
        evDiffs[e][bn] = ents;
      }
    }
  }

  if (evDiffs.empty()) {
    std::cout << Color::BOLD_GREEN << "✓ No differences found\n"
              << Color::RESET;
  } else {
    for (const auto &[ei, bds] : evDiffs) {
      std::cout << "\n" << createEventHeader(ei) << "\n\n";
      std::map<std::string, std::vector<std::string>> colls;
      for (const auto &[bn, _] : bds)
        colls[extractCollectionName(bn)].push_back(bn);

      for (const auto &[cn, brs] : colls) {
        std::cout << createCollectionHeader(cn) << "\n\n";

        // Collect all diff entries for this collection
        std::vector<std::pair<std::string, std::vector<DiffEntry>>> branchData;
        for (const auto &bn : brs) {
          branchData.push_back({bn, bds.at(bn)});
        }

        // Create aligned panels
        std::vector<std::string> pans =
            createAlignedPanels(branchData, COMPACT);

        if (!pans.empty()) {
          printPanelsInColumns(pans);
        }
        std::cout << "\n";
      }
    }
  }

  f1->Close();
  f2->Close();
  delete f1;
  delete f2;
  return 0;
}
