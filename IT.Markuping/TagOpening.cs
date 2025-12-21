using System;
using System.Diagnostics;

namespace IT.Markuping;

[DebuggerDisplay("{ToString(),nq}")]
public readonly struct TagOpening : IComparable<TagOpening>, IEquatable<TagOpening>, IFormattable
#if NET6_0_OR_GREATER
, ISpanFormattable
#endif
{
    private readonly int _start;
    private readonly int _end;

    #region Props

    public int Start => _start < 0 ? ~_start : _start;

    public int End => _end < 0 ? ~_end : _end;

    public int Length => End - Start;

    public bool IsEmpty => _start == _end;

    public Range Range => new(Start, End);

    public bool HasAttributes => _start < 0;

    public bool IsSelfClosing => _end < 0;

    #endregion Props

    #region Ctors

    private TagOpening(int start, int end, int offset)
    {
        if (end < 0)
        {
            _end = checked(end - offset);
            if (_end >= -1) throw new ArgumentOutOfRangeException(nameof(offset));
        }
        else
        {
            _end = checked(end + offset);
            if (_end < 1) throw new ArgumentOutOfRangeException(nameof(offset));
        }

        if (start < 0)
        {
            _start = checked(start - offset);
            if (_start >= 0) throw new ArgumentOutOfRangeException(nameof(offset));
        }
        else
        {
            _start = checked(start + offset);
            if (_start < 0) throw new ArgumentOutOfRangeException(nameof(offset));
        }
    }

    internal TagOpening(int start, int end)
    {
        _start = start;
        _end = end;
    }

    public TagOpening(int start, int end, bool hasAttributes, bool isSelfClosing)
    {
        if (start < 0) throw new ArgumentOutOfRangeException(nameof(start));
        if (end <= start) throw new ArgumentOutOfRangeException(nameof(end));

        _start = hasAttributes ? ~start : start;
        _end = isSelfClosing ? ~end : end;
    }

    #endregion Ctors

    public TagOpening AddOffset(int offset) => new(_start, _end, offset);

    #region Comparison

    public int CompareTo(TagOpening other)
    {
        var compared = Start.CompareTo(other.Start);

        Debug.Assert(End.CompareTo(other.End) == compared);

        return compared;
    }

    public bool Equals(TagOpening other) => _start == other._start && _end == other._end;

    public override bool Equals(object? obj) => obj is TagOpening tag && Equals(tag);

    public override int GetHashCode() => HashCode.Combine(_start, _end);

    #endregion Comparison

    #region ToString

    public override string ToString()
    {
        Span<char> span = stackalloc char[5 + (2 * 10)];

        var status = TryFormat(span, out var written);

        Debug.Assert(status);

        return new(span.Slice(0, written));
    }

    public bool TryFormat(Span<char> chars, out int written)
    {
        var minLength = IsSelfClosing ? 5 : 4;
        //<0..3>
        //<10..13/>
        //min-max = 6-25
        if (chars.Length >= minLength + 2 && ((uint)Start).TryFormat(chars.Slice(1), out var startWritten))
        {
            if (chars.Length >= minLength + startWritten + startWritten &&
                ((uint)End).TryFormat(chars.Slice(3 + startWritten), out var endWritten))
            {
                written = minLength + startWritten + endWritten;
                if (chars.Length >= written)
                {
                    chars[0] = '<';
                    chars[startWritten + 1] = '.';
                    chars[startWritten + 2] = '.';
                    if (minLength == 5) chars[written - 2] = '/';
                    chars[written - 1] = '>';
                    return true;
                }
                else
                {
                    //TODO: clear startWritten and endWritten
                    //chars.Slice(1, startWritten + 2 + endWritten).Clear();
                }
            }
            else
            {
                //TODO: clear startWritten
                //chars.Slice(1, startWritten).Clear();
            }
        }
        written = 0;
        return false;
    }

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

    public static bool operator ==(TagOpening left, TagOpening right) => left.Equals(right);

    public static bool operator !=(TagOpening left, TagOpening right) => !left.Equals(right);

    public static bool operator <(TagOpening left, TagOpening right) => left.CompareTo(right) < 0;

    public static bool operator <=(TagOpening left, TagOpening right) => left.CompareTo(right) <= 0;

    public static bool operator >(TagOpening left, TagOpening right) => left.CompareTo(right) > 0;

    public static bool operator >=(TagOpening left, TagOpening right) => left.CompareTo(right) >= 0;

    public static implicit operator Tag(TagOpening opening) => new(opening._start, opening._end);

    #endregion Operators
}