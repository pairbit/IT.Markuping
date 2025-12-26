using System;
using System.Diagnostics;

namespace IT.Markuping;

[DebuggerDisplay("{ToString(),nq}")]
public readonly struct TagNS : IComparable<TagNS>, IEquatable<TagNS>, IFormattable
#if NET6_0_OR_GREATER
, ISpanFormattable
#endif
{
    private readonly int _start;
    private readonly int _end;

    #region Props

    public int Start => _start;

    public int End => _end;

    public int Length => End - Start;

    public bool IsEmpty => _start == _end;

    public Range Range => new(Start, End);

    #endregion Props

    #region Ctors

    private TagNS(int start, int end, int offset)
    {
        _end = checked(end + offset);
        if (_end < 1) throw new ArgumentOutOfRangeException(nameof(offset));

        _start = checked(start + offset);
        if (_start < 0) throw new ArgumentOutOfRangeException(nameof(offset));
    }

    public TagNS(int start, int end)
    {
        if (start < 0) throw new ArgumentOutOfRangeException(nameof(start));
        if (end <= start) throw new ArgumentOutOfRangeException(nameof(end));

        _start = start;
        _end = end;
    }

    #endregion Ctors

    public TagNS AddOffset(int offset) => new(_start, _end, offset);

    public TagNS MultipleOffset(int offset)
        => new(checked(_start * offset), checked(_end * offset));

    #region Comparison

    public int CompareTo(TagNS other)
    {
        var compared = Start.CompareTo(other.Start);

        Debug.Assert(End.CompareTo(other.End) == compared);

        return compared;
    }

    public bool Equals(TagNS other) => _start == other._start && _end == other._end;

    public override bool Equals(object? obj) => obj is TagNS tag && Equals(tag);

    public override int GetHashCode() => HashCode.Combine(_start, _end);

    #endregion Comparison

    #region ToString

    public override string ToString()
    {
#if NETSTANDARD2_0
        return $"{Start}..{End}";
#else
        Span<char> span = stackalloc char[2 + (2 * 10)];

        var status = TryFormat(span, out var written);

        Debug.Assert(status);

        return new(span.Slice(0, written));
#endif
    }

#if !NETSTANDARD2_0
    public bool TryFormat(Span<char> chars, out int written, bool clear = true)
    {
        var minLength = 2;
        //0..3
        //min-max = 4-22
        if (chars.Length >= minLength + 2 && ((uint)Start).TryFormat(chars, out var startWritten))
        {
            if (chars.Length >= minLength + startWritten + startWritten &&
                ((uint)End).TryFormat(chars.Slice(2 + startWritten), out var endWritten))
            {
                written = minLength + startWritten + endWritten;
                chars[startWritten] = '.';
                chars[startWritten + 1] = '.';
                return true;
            }
            else if (clear)
            {
                chars.Slice(0, startWritten).Clear();
            }
        }
        written = 0;
        return false;
    }
#endif

    #endregion ToString

    #region Formattable

    string IFormattable.ToString(string? format, IFormatProvider? formatProvider)
        => ToString();

#if NET6_0_OR_GREATER
    bool ISpanFormattable.TryFormat(Span<char> destination, out int charsWritten, ReadOnlySpan<char> format, IFormatProvider? provider)
    {
        if (format.Length != 0) throw new FormatException();

        return TryFormat(destination, out charsWritten);
    }
#endif

    #endregion Formattable

    #region Operators

    public static bool operator ==(TagNS left, TagNS right) => left.Equals(right);

    public static bool operator !=(TagNS left, TagNS right) => !left.Equals(right);

    public static bool operator <(TagNS left, TagNS right) => left.CompareTo(right) < 0;

    public static bool operator <=(TagNS left, TagNS right) => left.CompareTo(right) <= 0;

    public static bool operator >(TagNS left, TagNS right) => left.CompareTo(right) > 0;

    public static bool operator >=(TagNS left, TagNS right) => left.CompareTo(right) >= 0;

    #endregion Operators
}