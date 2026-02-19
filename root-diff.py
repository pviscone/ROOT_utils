#!/usr/bin/env python3
"""
CLI tool to diff two ROOT files branch by branch, event by event.
"""
import argparse
import re
import sys
from typing import List, Pattern, Dict
from collections import defaultdict

from rich.console import Console, Group
from rich.panel import Panel
from rich.table import Table
from rich.columns import Columns
from rich import box
from rich.text import Text

# Global debug flag
DEBUG = False
# Global tolerance settings
ABS_TOLERANCE = None
REL_TOLERANCE = None
# Compact mode
COMPACT = False
# Rich console
console = Console()


def debug_print(*args, **kwargs):
    """Print debug messages only if DEBUG is enabled."""
    if DEBUG:
        console.print("[bold yellow][DEBUG][/bold yellow]", *args, **kwargs, file=sys.stderr)


def parse_args():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description="Diff two ROOT files branch by branch, event by event"
    )
    parser.add_argument("file1", help="First ROOT file")
    parser.add_argument("file2", help="Second ROOT file")
    parser.add_argument(
        "-t", "--tree",
        default="Events",
        help="Name of the TTree (default: Events)"
    )
    parser.add_argument(
        "-b", "--branches",
        default=".*",
        help="Comma-separated regex patterns to match branch names (default: .*)"
    )
    parser.add_argument(
        "-n", "--num-events",
        type=int,
        default=-1,
        help="Number of events to compare (default: -1, all events)"
    )
    parser.add_argument(
        "-s", "--start",
        type=int,
        default=0,
        help="Start from event number (default: 0)"
    )
    parser.add_argument(
        "--abs-tolerance",
        type=float,
        default=None,
        help="Absolute tolerance for floating point comparisons"
    )
    parser.add_argument(
        "--rel-tolerance",
        type=float,
        default=None,
        help="Relative tolerance for floating point comparisons"
    )
    parser.add_argument(
        "--compact",
        action="store_true",
        help="Show only differing elements (default: show full collection)"
    )
    parser.add_argument(
        "--debug",
        action="store_true",
        help="Enable debug output"
    )
    
    args = parser.parse_args()
    
    # Check that only one tolerance is specified
    if args.abs_tolerance is not None and args.rel_tolerance is not None:
        parser.error("Cannot specify both --abs-tolerance and --rel-tolerance at the same time")
    
    return args


def extract_collection_name(branch_name: str) -> str:
    """
    Extract collection name from branch name.
    E.g., 'Electron_pt' -> 'Electron', 'nJet' -> 'Other'
    """
    # Check for common patterns: Collection_property
    if '_' in branch_name:
        parts = branch_name.split('_')
        # Return the first part as collection name
        return parts[0]
    else:
        # Branches without underscore go to "Other" category
        return "Other"


def compile_branch_patterns(patterns_str: str) -> List[Pattern]:
    """Compile comma-separated regex patterns."""
    debug_print(f"compile_branch_patterns: input='{patterns_str}'")
    patterns = [p.strip() for p in patterns_str.split(",")]
    debug_print(f"compile_branch_patterns: split patterns={patterns}")
    compiled = [re.compile(p) for p in patterns if p]
    debug_print(f"compile_branch_patterns: compiled {len(compiled)} pattern(s)")
    return compiled


def matches_patterns(branch_name: str, patterns: List[Pattern]) -> bool:
    """Check if branch name matches any of the patterns."""
    result = any(pattern.search(branch_name) for pattern in patterns)
    debug_print(f"matches_patterns: branch='{branch_name}' -> {result}")
    return result


def compare_values(val1, val2) -> bool:
    """Compare two values, handling arrays and scalars."""
    import numpy as np
    
    debug_print(f"compare_values: type1={type(val1)}, type2={type(val2)}")
    
    # Handle NaN values
    try:
        if np.isnan(val1) and np.isnan(val2):
            debug_print("compare_values: both NaN -> True")
            return True
    except (TypeError, ValueError):
        pass
    
    # Compare arrays
    if isinstance(val1, np.ndarray) and isinstance(val2, np.ndarray):
        debug_print(f"compare_values: comparing arrays, shape1={val1.shape}, shape2={val2.shape}")
        if val1.shape != val2.shape:
            debug_print("compare_values: different shapes -> False")
            return False
        # Use tolerance-aware comparison for floating point
        if np.issubdtype(val1.dtype, np.floating) or np.issubdtype(val2.dtype, np.floating):
            if ABS_TOLERANCE is not None:
                result = np.allclose(val1, val2, atol=ABS_TOLERANCE, rtol=0, equal_nan=True)
                debug_print(f"compare_values: floating point arrays with atol={ABS_TOLERANCE} -> {result}")
            elif REL_TOLERANCE is not None:
                result = np.allclose(val1, val2, atol=0, rtol=REL_TOLERANCE, equal_nan=True)
                debug_print(f"compare_values: floating point arrays with rtol={REL_TOLERANCE} -> {result}")
            else:
                result = np.allclose(val1, val2, equal_nan=True)
                debug_print(f"compare_values: floating point arrays (default) -> {result}")
            return result
        result = np.array_equal(val1, val2)
        debug_print(f"compare_values: non-floating arrays -> {result}")
        return result
    
    # Compare scalars
    try:
        if np.issubdtype(type(val1), np.floating) or np.issubdtype(type(val2), np.floating):
            if ABS_TOLERANCE is not None:
                result = np.isclose(val1, val2, atol=ABS_TOLERANCE, rtol=0, equal_nan=True)
                debug_print(f"compare_values: floating point scalars with atol={ABS_TOLERANCE} -> {result}")
            elif REL_TOLERANCE is not None:
                result = np.isclose(val1, val2, atol=0, rtol=REL_TOLERANCE, equal_nan=True)
                debug_print(f"compare_values: floating point scalars with rtol={REL_TOLERANCE} -> {result}")
            else:
                result = np.isclose(val1, val2, equal_nan=True)
                debug_print(f"compare_values: floating point scalars (default) -> {result}")
            return result
    except (TypeError, ValueError):
        pass
    
    result = val1 == val2
    debug_print(f"compare_values: default comparison -> {result}")
    return result


def is_array_like(val):
    """Check if value is array-like."""
    import numpy as np
    result = isinstance(val, (list, np.ndarray)) and (not isinstance(val, np.ndarray) or val.ndim > 0)
    debug_print(f"is_array_like: type={type(val)}, result={result}")
    return result


def format_value_for_alignment(val):
    """
    Format a value for display with alignment information.
    Returns (formatted_string, is_float)
    """
    if val is None:
        return ("*", False)
    
    if isinstance(val, float):
        # Format with consistent precision
        return (f"{val:.6g}", True)
    return (str(val), False)


def align_numeric_strings(strings, is_floats):
    """
    Align numeric strings so decimal points line up.
    Returns list of aligned strings with consistent width.
    """
    if not strings:
        return strings
    
    # Check if we have any floats
    has_floats = any(is_floats)
    
    if not has_floats:
        # No floats, just right-align
        max_width = max(len(s) for s in strings)
        return [s.rjust(max_width) for s in strings]
    
    # Parse strings to find decimal point positions
    parts = []
    for s, is_float in zip(strings, is_floats):
        if s == "*":
            parts.append((s, 0, 0))
        elif '.' in s:
            integer, decimal = s.split('.')
            parts.append((s, len(integer), len(decimal)))
        elif 'e' in s.lower():
            # Scientific notation - treat as special case
            parts.append((s, len(s), 0))
        else:
            # Integer or other
            parts.append((s, len(s), 0))
    
    # Find max widths for integer and decimal parts
    max_int = max(p[1] for p in parts)
    max_dec = max(p[2] for p in parts)
    
    # Align all strings
    aligned = []
    for s, int_len, dec_len in parts:
        if s == "*":
            # Center the asterisk in the space
            total_width = max_int + (1 + max_dec if max_dec > 0 else 0)
            aligned.append(s.rjust(total_width))
        elif 'e' in s.lower():
            # Scientific notation - right align to total width
            total_width = max_int + (1 + max_dec if max_dec > 0 else 0)
            aligned.append(s.rjust(total_width))
        elif '.' in s:
            # Has decimal point
            integer, decimal = s.split('.')
            padded_int = integer.rjust(max_int)
            padded_dec = decimal.ljust(max_dec)
            aligned.append(f"{padded_int}.{padded_dec}")
        else:
            # No decimal point
            if max_dec > 0:
                # Add spaces for where decimal would be
                aligned.append(s.rjust(max_int) + ' ' * (1 + max_dec))
            else:
                aligned.append(s.rjust(max_int))
    
    return aligned


def diff_events(tree1, tree2, branch_name: str, start: int, num_events: int):
    """
    Diff a single branch across events and yield differences.
    
    Yields tuples of (event_number, all_tuples, has_diff) where:
    - all_tuples is a list of tuples (index, val1, val2, is_different)
    - has_diff indicates if there's at least one difference
    """
    import numpy as np
    
    debug_print(f"diff_events: branch='{branch_name}', start={start}, num_events={num_events}")
    
    try:
        # Read the branch data
        debug_print(f"diff_events: reading branch '{branch_name}' from both trees")
        data1 = tree1[branch_name].array(library="np")
        data2 = tree2[branch_name].array(library="np")
        debug_print(f"diff_events: data1 len={len(data1)}, data2 len={len(data2)}")
    except Exception as e:
        debug_print(f"diff_events: ERROR reading branch '{branch_name}': {e}")
        console.print(f"[yellow]Warning: Could not read branch '{branch_name}': {e}[/yellow]", file=sys.stderr)
        return
    
    # Determine the range of events to compare
    max_events = min(len(data1), len(data2))
    end = max_events if num_events == -1 else min(start + num_events, max_events)
    debug_print(f"diff_events: max_events={max_events}, end={end}")
    
    for event_idx in range(start, end):
        if event_idx >= len(data1) or event_idx >= len(data2):
            debug_print(f"diff_events: event {event_idx} out of range, breaking")
            break
            
        val1 = data1[event_idx]
        val2 = data2[event_idx]
        
        debug_print(f"diff_events: event {event_idx}, val1={val1}, val2={val2}")
        
        all_tuples = []
        has_diff = False
        
        # Check if values are array-like (jagged arrays or regular arrays)
        is_arr1 = is_array_like(val1)
        is_arr2 = is_array_like(val2)
        
        debug_print(f"diff_events: event {event_idx}, is_arr1={is_arr1}, is_arr2={is_arr2}")
        
        if is_arr1 or is_arr2:
            # Convert to lists for easier handling
            try:
                list1 = val1.tolist() if isinstance(val1, np.ndarray) else list(val1) if is_arr1 else [val1]
                list2 = val2.tolist() if isinstance(val2, np.ndarray) else list(val2) if is_arr2 else [val2]
            except (AttributeError, TypeError):
                list1 = [val1] if not is_arr1 else list(val1)
                list2 = [val2] if not is_arr2 else list(val2)
            
            debug_print(f"diff_events: event {event_idx}, list1 len={len(list1)}, list2 len={len(list2)}")
            
            max_len = max(len(list1), len(list2))
            
            for i in range(max_len):
                v1 = list1[i] if i < len(list1) else None
                v2 = list2[i] if i < len(list2) else None
                
                debug_print(f"diff_events: event {event_idx}, element {i}, v1={v1}, v2={v2}")
                
                if v1 is None:
                    all_tuples.append((i, None, v2, True))
                    has_diff = True
                    debug_print(f"diff_events: element {i} missing in list1")
                elif v2 is None:
                    all_tuples.append((i, v1, None, True))
                    has_diff = True
                    debug_print(f"diff_events: element {i} missing in list2")
                else:
                    # Compare individual elements
                    try:
                        is_different = not compare_values(v1, v2)
                        all_tuples.append((i, v1, v2, is_different))
                        if is_different:
                            has_diff = True
                            debug_print(f"diff_events: element {i} different")
                    except Exception as e:
                        debug_print(f"diff_events: comparison exception for element {i}: {e}")
                        # If comparison fails, treat as different
                        is_different = v1 != v2
                        all_tuples.append((i, v1, v2, is_different))
                        if is_different:
                            has_diff = True
        else:
            # Scalar values (index 0 for single values)
            debug_print(f"diff_events: event {event_idx}, comparing scalars")
            try:
                is_different = not compare_values(val1, val2)
                all_tuples.append((0, val1, val2, is_different))
                if is_different:
                    has_diff = True
                    debug_print(f"diff_events: event {event_idx}, scalar different")
            except Exception as e:
                debug_print(f"diff_events: comparison exception for event {event_idx}: {e}")
                # If comparison fails, treat as different
                is_different = val1 != val2
                all_tuples.append((0, val1, val2, is_different))
                if is_different:
                    has_diff = True
        
        if has_diff:
            debug_print(f"diff_events: event {event_idx}, has differences")
            yield (event_idx, all_tuples, has_diff)
        else:
            debug_print(f"diff_events: event {event_idx}, no differences")


def create_branch_panel(branch_name: str, all_tuples: List, compact: bool) -> Panel:
    """Create a panel for a single branch showing its differences."""
    
    # Filter based on mode
    if compact:
        # Show only different elements with their indices
        tuples_to_show = [(idx, v1, v2, is_diff) for idx, v1, v2, is_diff in all_tuples if is_diff]
    else:
        # Show all elements
        tuples_to_show = all_tuples
    
    if not tuples_to_show:
        return Panel("No data", title=f"[blue]{branch_name}[/blue]", border_style="blue", box=box.ROUNDED)
    
    lines = []
    
    if compact:
        # Calculate max index width for alignment
        max_idx_width = len(str(max(idx for idx, _, _, _ in tuples_to_show))) if tuples_to_show else 1
        
        # Format values and track if they're floats
        val1_data = [format_value_for_alignment(v1) for idx, v1, v2, _ in tuples_to_show]
        val2_data = [format_value_for_alignment(v2) for idx, v1, v2, _ in tuples_to_show]
        
        val1_strs = [d[0] for d in val1_data]
        val1_is_floats = [d[1] for d in val1_data]
        
        val2_strs = [d[0] for d in val2_data]
        val2_is_floats = [d[1] for d in val2_data]
        
        # Align the values
        aligned_val1 = align_numeric_strings(val1_strs, val1_is_floats)
        aligned_val2 = align_numeric_strings(val2_strs, val2_is_floats)
        
        # Build lines with index, proper alignment and colors
        for (idx, v1, v2, is_diff), v1_str, v2_str in zip(tuples_to_show, aligned_val1, aligned_val2):
            # Color code the values
            if v1_str.strip() == "*":
                v1_colored = f"[red]{v1_str}[/red]"
            else:
                v1_colored = f"[yellow]{v1_str}[/yellow]"
            
            if v2_str.strip() == "*":
                v2_colored = f"[red]{v2_str}[/red]"
            else:
                v2_colored = f"[green]{v2_str}[/green]"
            
            arrow = "[dim cyan]->[/dim cyan]"
            idx_str = f"[dim cyan][{idx:>{max_idx_width}}][/dim cyan]"
            
            lines.append(f"{idx_str} {v1_colored}  {arrow}  {v2_colored}")
    else:
        # Full mode - no indices
        # Format values and track if they're floats
        val1_data = [format_value_for_alignment(v1) for idx, v1, v2, _ in tuples_to_show]
        val2_data = [format_value_for_alignment(v2) for idx, v1, v2, _ in tuples_to_show]
        
        val1_strs = [d[0] for d in val1_data]
        val1_is_floats = [d[1] for d in val1_data]
        
        val2_strs = [d[0] for d in val2_data]
        val2_is_floats = [d[1] for d in val2_data]
        
        # Align the values
        aligned_val1 = align_numeric_strings(val1_strs, val1_is_floats)
        aligned_val2 = align_numeric_strings(val2_strs, val2_is_floats)
        
        # Build lines with proper alignment and colors
        for (idx, v1, v2, is_diff), v1_str, v2_str in zip(tuples_to_show, aligned_val1, aligned_val2):
            # Color code the values
            if v1_str.strip() == "*":
                v1_colored = f"[red]{v1_str}[/red]"
            elif is_diff:
                v1_colored = f"[yellow]{v1_str}[/yellow]"
            else:
                v1_colored = f"[dim]{v1_str}[/dim]"
            
            if v2_str.strip() == "*":
                v2_colored = f"[red]{v2_str}[/red]"
            elif is_diff:
                v2_colored = f"[green]{v2_str}[/green]"
            else:
                v2_colored = f"[dim]{v2_str}[/dim]"
            
            # Show arrow only if different
            if is_diff:
                arrow = "[dim cyan]->[/dim cyan]"
            else:
                arrow = "[dim]  [/dim]"
            
            lines.append(f"{v1_colored}  {arrow}  {v2_colored}")
    
    content = "\n".join(lines)
    
    return Panel(
        content,
        title=f"[bold blue]{branch_name}[/bold blue]",
        border_style="blue",
        box=box.ROUNDED,
        expand=False
    )


def create_event_header(event_idx: int) -> str:
    """Create a full-width event header."""
    terminal_width = console.width
    event_text = f" Event {event_idx} "
    
    # Calculate padding
    text_len = len(event_text)
    remaining = terminal_width - text_len - 2  # -2 for the corner characters
    
    if remaining < 0:
        remaining = 0
    
    left_padding = remaining // 2
    right_padding = remaining - left_padding
    
    # Build the header
    header = "╔" + "═" * left_padding + event_text + "═" * right_padding + "╗"
    
    return f"[bold magenta]{header}[/bold magenta]"


def create_collection_header(collection_name: str) -> str:
    """Create a collection header."""
    return f"[bold cyan]──── {collection_name} ────[/bold cyan]"


def main():
    """Main function."""
    global DEBUG, ABS_TOLERANCE, REL_TOLERANCE, COMPACT
    
    args = parse_args()
    DEBUG = args.debug
    ABS_TOLERANCE = args.abs_tolerance
    REL_TOLERANCE = args.rel_tolerance
    COMPACT = args.compact
    
    debug_print("=== Starting ROOT file diff ===")
    debug_print(f"Arguments: {args}")
    debug_print(f"Absolute tolerance: {ABS_TOLERANCE}")
    debug_print(f"Relative tolerance: {REL_TOLERANCE}")
    debug_print(f"Compact mode: {COMPACT}")
    
    # Check for uproot
    try:
        import uproot
        debug_print("uproot imported successfully")
    except ImportError:
        console.print("[bold red]Error:[/bold red] uproot is required. Install with: pip install uproot", file=sys.stderr)
        sys.exit(1)
    
    # Compile branch patterns
    patterns = compile_branch_patterns(args.branches)
    
    # Open ROOT files
    try:
        debug_print(f"Opening file1: {args.file1}")
        file1 = uproot.open(args.file1)
        debug_print(f"Opening file2: {args.file2}")
        file2 = uproot.open(args.file2)
    except Exception as e:
        debug_print(f"Error opening files: {e}")
        console.print(f"[bold red]Error opening ROOT files:[/bold red] {e}", file=sys.stderr)
        sys.exit(1)
    
    # Get trees
    try:
        debug_print(f"Getting tree '{args.tree}' from file1")
        tree1 = file1[args.tree]
        debug_print(f"Getting tree '{args.tree}' from file2")
        tree2 = file2[args.tree]
        debug_print(f"Tree1 entries: {tree1.num_entries}, Tree2 entries: {tree2.num_entries}")
    except Exception as e:
        debug_print(f"Error accessing tree: {e}")
        console.print(f"[bold red]Error accessing tree '{args.tree}':[/bold red] {e}", file=sys.stderr)
        sys.exit(1)
    
    # Get matching branches
    branches1 = set(tree1.keys())
    branches2 = set(tree2.keys())
    debug_print(f"File1 has {len(branches1)} branches")
    debug_print(f"File2 has {len(branches2)} branches")
    
    all_branches = branches1 | branches2
    debug_print(f"Total unique branches: {len(all_branches)}")
    
    matching_branches = sorted([
        b for b in all_branches if matches_patterns(b, patterns)
    ])
    debug_print(f"Matching branches after pattern filtering: {len(matching_branches)}")
    
    if not matching_branches:
        console.print("[bold red]No branches match the specified patterns[/bold red]", file=sys.stderr)
        sys.exit(1)
    
    # Identify missing branches
    missing_in_file1 = sorted([b for b in matching_branches if b not in branches1])
    missing_in_file2 = sorted([b for b in matching_branches if b not in branches2])
    debug_print(f"Missing in file1: {len(missing_in_file1)}")
    debug_print(f"Missing in file2: {len(missing_in_file2)}")
    
    # Only compare branches that exist in both files
    common_branches = sorted([b for b in matching_branches if b in branches1 and b in branches2])
    debug_print(f"Common branches to compare: {len(common_branches)}")
    debug_print(f"Common branches: {common_branches}")
    
    # Print header information
    header_text = f"[bold cyan]Comparing files:[/bold cyan]\n"
    header_text += f"  [green]File 1:[/green] {args.file1}\n"
    header_text += f"  [green]File 2:[/green] {args.file2}\n"
    header_text += f"  [green]Tree:[/green] {args.tree}\n"
    header_text += f"  [green]Mode:[/green] {'Compact' if COMPACT else 'Full collection'}"
    if ABS_TOLERANCE is not None:
        header_text += f"\n  [green]Absolute tolerance:[/green] {ABS_TOLERANCE}"
    if REL_TOLERANCE is not None:
        header_text += f"\n  [green]Relative tolerance:[/green] {REL_TOLERANCE}"
    
    console.print(Panel(header_text, box=box.ROUNDED, border_style="cyan"))
    
    # Print missing branches
    if missing_in_file1 or missing_in_file2:
        console.print("\n[bold yellow]═══ Missing Branches ═══[/bold yellow]")
        if missing_in_file1:
            console.print(f"\n[red]Missing in {args.file1}:[/red]")
            for branch in missing_in_file1:
                console.print(f"  [red]✗[/red] {branch}")
        if missing_in_file2:
            console.print(f"\n[red]Missing in {args.file2}:[/red]")
            for branch in missing_in_file2:
                console.print(f"  [red]✗[/red] {branch}")
        console.print()
    
    # Print inspected branches
    if common_branches:
        console.print("[bold cyan]═══ Inspected Branches ═══[/bold cyan]")
        console.print(f"\n[dim]Comparing {len(common_branches)} branch(es):[/dim]")
        for branch in common_branches:
            console.print(f"  [green]✓[/green] {branch}")
        console.print()
    
    console.print("[cyan]" + "═" * console.width + "[/cyan]\n")
    
    # Determine event range
    num_events1 = tree1.num_entries
    num_events2 = tree2.num_entries
    max_events = min(num_events1, num_events2)
    
    end_event = max_events if args.num_events == -1 else min(args.start + args.num_events, max_events)
    debug_print(f"Event range: start={args.start}, end={end_event}, max={max_events}")
    
    if args.start >= max_events:
        console.print(f"[bold red]Error: Start event {args.start} is beyond available events ({max_events})[/bold red]", file=sys.stderr)
        sys.exit(1)
    
    # Collect all differences organized by event
    event_diffs = {}  # event_number -> {branch_name -> [all_tuples]}
    
    debug_print("Starting branch comparison...")
    for branch in common_branches:
        debug_print(f"Processing branch: {branch}")
        # Compare branch values
        for event_idx, all_tuples, has_diff in diff_events(tree1, tree2, branch, args.start, args.num_events):
            if event_idx not in event_diffs:
                event_diffs[event_idx] = {}
            event_diffs[event_idx][branch] = all_tuples
    
    debug_print(f"Total events with differences: {len(event_diffs)}")
    
    # Print output organized by event
    if not event_diffs:
        console.print(Panel("[bold green]✓ No differences found between the files.[/bold green]", 
                          box=box.ROUNDED, border_style="green"))
    else:
        for event_idx in sorted(event_diffs.keys()):
            # Print full-width event header
            console.print()
            console.print(create_event_header(event_idx))
            console.print()
            
            # Group branches by collection
            collections = defaultdict(list)
            for branch in sorted(event_diffs[event_idx].keys()):
                collection = extract_collection_name(branch)
                collections[collection].append(branch)
            
            # Display each collection separately
            for collection in sorted(collections.keys()):
                # Print collection header
                console.print(create_collection_header(collection))
                console.print()
                
                # Create panels for branches in this collection
                panels = []
                for branch in collections[collection]:
                    all_tuples = event_diffs[event_idx][branch]
                    panel = create_branch_panel(branch, all_tuples, COMPACT)
                    panels.append(panel)
                
                # Display panels in columns (side by side)
                console.print(Columns(panels, equal=False, expand=False))
                console.print()
            
    debug_print("=== Finished ROOT file diff ===")


if __name__ == "__main__":
    main()
