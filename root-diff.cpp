// root_diff.cpp - Complete Working Version
// Compile with: g++ -o root_diff root_diff.cpp `root-config --cflags --libs`
// -std=c++17

#include <algorithm>
#include <cmath>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <limits>
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
    if (std::isinf(val1) && std::isinf(val2))
      return std::signbit(val1) == std::signbit(val2);
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
    if (std::isinf(d1) && std::isinf(d2))
      return std::signbit(d1) == std::signbit(d2);
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
  std::vector<std::string> vals;
  std::vector<bool> isFloats;
  bool isDifferent;
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
    if (s == "True" || s == "False")
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
      int leftPad = std::max(0, maxInt - 1);
      std::string tail = (maxDec > 0) ? std::string(1 + maxDec, ' ') : "";
      result.push_back(std::string(leftPad, ' ') + "*" + tail);
    } else if (s == "True" || s == "False") {
      int leftPad = std::max(0, maxInt - 1);
      int totalWidth = maxInt + (maxDec > 0 ? 1 + maxDec : 0);
      int rightPad = std::max(0, totalWidth - leftPad - (int)s.length());
      result.push_back(std::string(leftPad, ' ') + s +
                       std::string(rightPad, ' '));
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
  virtual size_t fileCount() const = 0;
};

template <typename T>
class ReaderScalarHandlerMulti : public BranchDiffHandler {
  std::string branchName;
  std::vector<std::unique_ptr<TTreeReaderValue<T>>> readers;

public:
  ReaderScalarHandlerMulti(const std::string &name,
                           const std::vector<TTreeReader *> &rs)
      : branchName(name) {
    readers.reserve(rs.size());
    for (auto *r : rs) {
      readers.push_back(
          std::make_unique<TTreeReaderValue<T>>(*r, name.c_str()));
    }
  }

  bool processEvent(std::vector<DiffEntry> &entries) override {
    std::vector<std::string> vals;
    std::vector<bool> isFloats;
    vals.reserve(readers.size());
    isFloats.reserve(readers.size());

    bool hasDiff = false;
    bool hasPrev = false;
    T prevVal{};

    for (const auto &reader : readers) {
      T v = *(*reader);
      vals.push_back(formatValue(v));
      isFloats.push_back(std::is_floating_point_v<T>);
      if (hasPrev && !compareValues(prevVal, v)) {
        hasDiff = true;
      }
      hasPrev = true;
      prevVal = v;
    }

    DiffEntry e{0, vals, isFloats, hasDiff};
    entries.push_back(e);
    return hasDiff;
  }

  std::string getBranchName() const override { return branchName; }
  size_t fileCount() const override { return readers.size(); }
};

template <typename T> class ReaderArrayHandlerMulti : public BranchDiffHandler {
  std::string branchName;
  std::vector<std::unique_ptr<TTreeReaderArray<T>>> readers;

public:
  ReaderArrayHandlerMulti(const std::string &name,
                          const std::vector<TTreeReader *> &rs)
      : branchName(name) {
    readers.reserve(rs.size());
    for (auto *r : rs) {
      readers.push_back(
          std::make_unique<TTreeReaderArray<T>>(*r, name.c_str()));
    }
  }

  bool processEvent(std::vector<DiffEntry> &entries) override {
    bool hasDiff = false;
    std::vector<size_t> sizes;
    sizes.reserve(readers.size());

    size_t maxSize = 0;
    for (const auto &reader : readers) {
      size_t sz = reader->GetSize();
      sizes.push_back(sz);
      maxSize = std::max(maxSize, sz);
    }

    for (size_t i = 0; i < maxSize; i++) {
      DiffEntry e;
      e.index = static_cast<int>(i);
      e.vals.resize(readers.size());
      e.isFloats.resize(readers.size(), std::is_floating_point_v<T>);

      bool rowDiff = false;
      bool hasPrev = false;
      bool prevMissing = false;
      T prevVal{};

      for (size_t r = 0; r < readers.size(); r++) {
        bool missing = i >= sizes[r];
        if (missing) {
          e.vals[r] = "*";
        } else {
          T v = (*readers[r])[i];
          e.vals[r] = formatValue(v);
          if (hasPrev && !prevMissing && !compareValues(prevVal, v)) {
            rowDiff = true;
          }
          prevVal = v;
        }

        if (hasPrev && prevMissing != missing) {
          rowDiff = true;
        }

        hasPrev = true;
        prevMissing = missing;
      }

      e.isDifferent = rowDiff;
      entries.push_back(e);
      if (rowDiff)
        hasDiff = true;
    }

    return hasDiff;
  }

  std::string getBranchName() const override { return branchName; }
  size_t fileCount() const override { return readers.size(); }
};

template <typename T>
std::unique_ptr<BranchDiffHandler>
makeArrayHandlerForTypeMulti(const std::string &branchName,
                             const std::vector<TTreeReader *> &readers) {
  return std::make_unique<ReaderArrayHandlerMulti<T>>(branchName, readers);
}

template <typename T>
std::unique_ptr<BranchDiffHandler>
makeScalarHandlerForTypeMulti(const std::string &branchName,
                              const std::vector<TTreeReader *> &readers) {
  return std::make_unique<ReaderScalarHandlerMulti<T>>(branchName, readers);
}

std::unique_ptr<BranchDiffHandler>
createReaderHandler(const std::string &branchName,
                    const std::vector<TTree *> &trees,
                    const std::vector<TTreeReader *> &readers) {
  if (trees.empty() || readers.size() != trees.size())
    return nullptr;

  for (auto *tree : trees) {
    if (!tree || !tree->GetBranch(branchName.c_str()))
      return nullptr;
  }

  std::string elemType0 = getElementType(trees[0], branchName);
  ElemTypeId typeId0 = parseElemTypeId(elemType0);
  bool isColl0 = isCollection(trees[0], branchName);

  if (typeId0 == ElemTypeId::Unknown)
    return nullptr;

  for (size_t i = 1; i < trees.size(); i++) {
    std::string elemType = getElementType(trees[i], branchName);
    ElemTypeId typeId = parseElemTypeId(elemType);
    bool isColl = isCollection(trees[i], branchName);

    if (typeId != typeId0 || isColl != isColl0) {
      debugPrint("Warning: Type or collection mismatch for " + branchName);
      return nullptr;
    }
  }

  if (isColl0) {
    switch (typeId0) {
    case ElemTypeId::Float:
      return makeArrayHandlerForTypeMulti<float>(branchName, readers);
    case ElemTypeId::Double:
      return makeArrayHandlerForTypeMulti<double>(branchName, readers);
    case ElemTypeId::Int:
      return makeArrayHandlerForTypeMulti<int>(branchName, readers);
    case ElemTypeId::Bool:
      return makeArrayHandlerForTypeMulti<bool>(branchName, readers);
    case ElemTypeId::Long:
      return makeArrayHandlerForTypeMulti<Long64_t>(branchName, readers);
    case ElemTypeId::Short:
      return makeArrayHandlerForTypeMulti<Short_t>(branchName, readers);
    case ElemTypeId::Char:
      return makeArrayHandlerForTypeMulti<Char_t>(branchName, readers);
    case ElemTypeId::UChar:
      return makeArrayHandlerForTypeMulti<UChar_t>(branchName, readers);
    case ElemTypeId::UInt:
      return makeArrayHandlerForTypeMulti<UInt_t>(branchName, readers);
    case ElemTypeId::UShort:
      return makeArrayHandlerForTypeMulti<UShort_t>(branchName, readers);
    default:
      return nullptr;
    }
  }

  switch (typeId0) {
  case ElemTypeId::Float:
    return makeScalarHandlerForTypeMulti<float>(branchName, readers);
  case ElemTypeId::Double:
    return makeScalarHandlerForTypeMulti<double>(branchName, readers);
  case ElemTypeId::Int:
    return makeScalarHandlerForTypeMulti<int>(branchName, readers);
  case ElemTypeId::Bool:
    return makeScalarHandlerForTypeMulti<bool>(branchName, readers);
  case ElemTypeId::Long:
    return makeScalarHandlerForTypeMulti<Long64_t>(branchName, readers);
  case ElemTypeId::Short:
    return makeScalarHandlerForTypeMulti<Short_t>(branchName, readers);
  case ElemTypeId::Char:
    return makeScalarHandlerForTypeMulti<Char_t>(branchName, readers);
  case ElemTypeId::UChar:
    return makeScalarHandlerForTypeMulti<UChar_t>(branchName, readers);
  case ElemTypeId::UInt:
    return makeScalarHandlerForTypeMulti<UInt_t>(branchName, readers);
  case ElemTypeId::UShort:
    return makeScalarHandlerForTypeMulti<UShort_t>(branchName, readers);
  default:
    return nullptr;
  }
}

void printUsage(const char *progName) {
  std::cout << "Usage: " << progName
            << " FILE1 FILE2 [FILE3 ...] [OPTIONS]\n\n";
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
    bool compact, size_t fileCount) {
  if (branchData.empty())
    return {};

  if (fileCount == 0)
    return {};

  // First pass: collect all values for alignment
  std::vector<std::vector<std::string>> allVals(fileCount);
  std::vector<std::vector<bool>> allIsFloats(fileCount);
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
      for (size_t i = 0; i < fileCount && i < e.vals.size(); i++) {
        allVals[i].push_back(e.vals[i]);
        allIsFloats[i].push_back(e.isFloats[i]);
      }
      if (compact) {
        maxIdxWidth =
            std::max(maxIdxWidth, (size_t)std::to_string(e.index).length());
      }
    }
  }

  // Align all values per column
  std::vector<std::vector<std::string>> alignedAllVals(fileCount);
  std::vector<size_t> maxValWidths(fileCount, 0);
  for (size_t i = 0; i < fileCount; i++) {
    alignedAllVals[i] = alignNumericStrings(allVals[i], allIsFloats[i]);
    if (!alignedAllVals[i].empty())
      maxValWidths[i] = alignedAllVals[i][0].length();
  }

  // Second pass: create panels with aligned values
  std::vector<std::string> panels;
  std::vector<size_t> valueIndex(fileCount, 0);

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

    size_t titleWidth = branchName.length();
    std::string coloredTitle;
    {
      size_t pos = branchName.find('_');
      if (pos == std::string::npos) {
        coloredTitle = Color::BOLD_BLUE + branchName + Color::RESET;
      } else {
        std::string prefix = branchName.substr(0, pos + 1);
        std::string suffix = branchName.substr(pos + 1);
        coloredTitle = Color::CYAN + prefix + Color::RESET + Color::BOLD_BLUE +
                       suffix + Color::RESET;
      }
    }

    size_t arrowWidth = 6; // "  ->  "
    size_t valuesWidth = 0;
    for (size_t i = 0; i < fileCount; i++)
      valuesWidth += maxValWidths[i];
    size_t contentWidth = idxWidth + valuesWidth +
                          arrowWidth * (fileCount > 0 ? fileCount - 1 : 0);
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

      for (size_t i = 0; i < fileCount; i++) {
        std::string valAligned = alignedAllVals[i][valueIndex[i]];
        bool isMissing = valAligned.find('*') != std::string::npos;
        if (isMissing) {
          ls << Color::RED << valAligned << Color::RESET;
        } else if (e.isDifferent) {
          if (i == fileCount - 1) {
            ls << Color::GREEN << valAligned << Color::RESET;
          } else {
            ls << Color::YELLOW << valAligned << Color::RESET;
          }
        } else {
          ls << Color::DIM << valAligned << Color::RESET;
        }
        lineLen += maxValWidths[i];
        valueIndex[i]++;

        if (i + 1 < fileCount) {
          if (e.isDifferent) {
            ls << "  " << Color::DIM << Color::CYAN << "->" << Color::RESET
               << "  ";
          } else {
            ls << std::string(arrowWidth, ' ');
          }
          lineLen += arrowWidth;
        }
      }

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
          << Color::BOLD << coloredTitle << Color::RESET << Color::BLUE
          << horizontalLine(rightPad) << "╮" << Color::RESET << "\n";

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

  std::vector<std::string> filePaths;
  std::string treeName = "Events", branchPattern = ".*";
  Long64_t numEvents = -1, startEvent = 0;

  for (int i = 1; i < argc; i++) {
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
    } else if (arg.rfind("-", 0) == 0) {
      std::cerr << Color::BOLD_RED << "Error: " << Color::RESET
                << "Unknown option: " << arg << "\n";
      printUsage(argv[0]);
      return 1;
    } else {
      filePaths.push_back(arg);
    }
  }

  if (ABS_TOLERANCE >= 0 && REL_TOLERANCE >= 0) {
    std::cerr << Color::BOLD_RED << "Error: Cannot use both tolerances\n"
              << Color::RESET;
    return 1;
  }

  if (filePaths.size() < 2) {
    std::cerr << Color::BOLD_RED << "Error: " << Color::RESET
              << "At least two files are required.\n";
    printUsage(argv[0]);
    return 1;
  }

  std::vector<TFile *> files;
  std::vector<TTree *> trees;
  files.reserve(filePaths.size());
  trees.reserve(filePaths.size());

  for (const auto &path : filePaths) {
    TFile *file = TFile::Open(path.c_str());
    if (!file || file->IsZombie()) {
      std::cerr << Color::BOLD_RED << "Error opening file: " << Color::RESET
                << path << "\n";
      if (file)
        file->Close();
      delete file;
      for (auto *f : files) {
        f->Close();
        delete f;
      }
      return 1;
    }
    TTree *tree = dynamic_cast<TTree *>(file->Get(treeName.c_str()));
    if (!tree) {
      std::cerr << Color::BOLD_RED << "Error: tree not found in "
                << Color::RESET << path << "\n";
      file->Close();
      delete file;
      for (auto *f : files) {
        f->Close();
        delete f;
      }
      return 1;
    }
    files.push_back(file);
    trees.push_back(tree);
  }

  auto patterns = compileBranchPatterns(branchPattern);
  std::vector<std::set<std::string>> branchesPerFile;
  branchesPerFile.resize(trees.size());

  std::set<std::string> allBranches;
  for (size_t i = 0; i < trees.size(); i++) {
    auto *list = trees[i]->GetListOfBranches();
    for (int j = 0; j < list->GetEntries(); j++) {
      std::string n = list->At(j)->GetName();
      if (matchesPatterns(n, patterns)) {
        branchesPerFile[i].insert(n);
        allBranches.insert(n);
      }
    }
  }

  std::set<std::string> common = allBranches;
  for (const auto &brs : branchesPerFile) {
    std::set<std::string> nextCommon;
    std::set_intersection(common.begin(), common.end(), brs.begin(), brs.end(),
                          std::inserter(nextCommon, nextCommon.begin()));
    common = std::move(nextCommon);
  }

  std::vector<std::set<std::string>> missingPerFile;
  missingPerFile.resize(branchesPerFile.size());
  for (size_t i = 0; i < branchesPerFile.size(); i++) {
    std::set_difference(
        allBranches.begin(), allBranches.end(), branchesPerFile[i].begin(),
        branchesPerFile[i].end(),
        std::inserter(missingPerFile[i], missingPerFile[i].begin()));
  }

  std::vector<std::string> infoLines;
  infoLines.push_back(Color::BOLD_CYAN + "Comparing files:" + Color::RESET);
  for (size_t i = 0; i < filePaths.size(); i++) {
    infoLines.push_back("  File " + std::to_string(i + 1) + ": " +
                        getBaseName(filePaths[i]));
  }
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
  for (size_t i = 0; i < filePaths.size(); i++) {
    std::cout << "Missing in " << getBaseName(filePaths[i]) << ":\n";
    if (missingPerFile[i].empty()) {
      std::cout << "  " << Color::BOLD_GREEN << "✓ None" << Color::RESET
                << "\n";
    } else {
      printCollectionBoxes(groupByCollection(missingPerFile[i]),
                           Color::RED + std::string("✗") + Color::RESET);
    }
    if (i + 1 < filePaths.size())
      std::cout << "\n";
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

  std::vector<std::unique_ptr<TTreeReader>> readers;
  std::vector<TTreeReader *> readerPtrs;
  readers.reserve(trees.size());
  readerPtrs.reserve(trees.size());
  for (auto *tree : trees) {
    readers.push_back(std::make_unique<TTreeReader>(tree));
    readerPtrs.push_back(readers.back().get());
  }

  std::map<std::string, std::unique_ptr<BranchDiffHandler>> handlers;
  for (const auto &b : common) {
    auto h = createReaderHandler(b, trees, readerPtrs);
    if (h)
      handlers[b] = std::move(h);
  }

  Long64_t maxEv = std::numeric_limits<Long64_t>::max();
  for (auto *tree : trees) {
    maxEv = std::min(maxEv, tree->GetEntries());
  }
  Long64_t endEv =
      (numEvents == -1) ? maxEv : std::min(startEvent + numEvents, maxEv);

  std::map<Long64_t, std::map<std::string, std::vector<DiffEntry>>> evDiffs;

  for (Long64_t e = startEvent; e < endEv; e++) {
    for (auto *r : readerPtrs) {
      r->SetEntry(e);
    }

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

        std::vector<std::pair<std::string, std::vector<DiffEntry>>> branchData;
        for (const auto &bn : brs) {
          branchData.push_back({bn, bds.at(bn)});
        }

        std::vector<std::string> pans =
            createAlignedPanels(branchData, COMPACT, filePaths.size());

        if (!pans.empty()) {
          printPanelsInColumns(pans);
        }
        std::cout << "\n";
      }
    }
  }

  for (auto *f : files) {
    f->Close();
    delete f;
  }

  return 0;
}
