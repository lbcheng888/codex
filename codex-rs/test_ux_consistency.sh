#!/bin/bash
# Claude Code UX Consistency Test Script

set -e

echo "🔍 Testing Claude Code UX Consistency..."

# Test 1: Verify color scheme alignment
echo "🎨 Testing color scheme..."
if grep -q "Rgb(13, 13, 13)" tui/src/theme.rs; then
    echo "✅ Dark background color matches Claude Code"
else
    echo "❌ Dark background color mismatch"
    exit 1
fi

if grep -q "Rgb(247, 144, 61)" tui/src/theme.rs; then
    echo "✅ Claude orange accent color found"
else
    echo "❌ Claude orange accent missing"
    exit 1
fi

# Test 2: Verify layout structure
echo "📐 Testing layout structure..."
if grep -q "Layout::default" tui/src/chatwidget.rs; then
    echo "✅ Layout system detected"
else
    echo "❌ Layout system missing"
    exit 1
fi

# Test 3: Verify component consistency
echo "🧩 Testing component consistency..."
if [ -f "tui/src/loading_indicator.rs" ]; then
    echo "✅ Loading indicator component exists"
else
    echo "❌ Loading indicator component missing"
    exit 1
fi

if [ -f "tui/src/status_bar.rs" ]; then
    echo "✅ Status bar component exists"
else
    echo "❌ Status bar component missing"
    exit 1
fi

# Test 4: Verify theme system
echo "🎯 Testing theme system..."
if grep -q "claude_inspired" tui/src/theme.rs; then
    echo "✅ Claude-inspired theme system detected"
else
    echo "❌ Claude-inspired theme system missing"
    exit 1
fi

# Test 5: Verify interaction patterns
echo "⌨️ Testing interaction patterns..."
if grep -q "KeyCode::Tab" tui/src/chatwidget.rs; then
    echo "✅ Tab-based focus switching detected"
else
    echo "❌ Tab-based focus switching missing"
    exit 1
fi

if grep -q "InputFocus" tui/src/chatwidget.rs; then
    echo "✅ Focus management system detected"
else
    echo "❌ Focus management system missing"
    exit 1
fi

# Test 6: Verify visual feedback
echo "🔔 Testing visual feedback..."
if grep -q "focus_indicator" tui/src/chatwidget.rs; then
    echo "✅ Focus indicator system detected"
else
    echo "❌ Focus indicator system missing"
    exit 1
fi

# Test 7: Verify responsive design
echo "📱 Testing responsive design..."
if grep -q "min.*height" tui/src/chatwidget.rs; then
    echo "✅ Responsive constraints detected"
else
    echo "❌ Responsive constraints missing"
    exit 1
fi

echo "🎉 All UX consistency tests passed!"
echo "✨ Claude Code UX alignment complete"

# Optional: Run a quick build test
echo "🔨 Testing build..."
cd tui && cargo check --quiet
echo "✅ Build test passed"