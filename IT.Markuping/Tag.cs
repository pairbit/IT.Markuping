using System;
using System.Diagnostics;

namespace IT.Markuping;

[DebuggerDisplay("{ToString(),nq}")]
public readonly struct Tag : IComparable<Tag>, IEquatable<Tag>, IFormattable
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

    public TagEnding Ending
    {
        get
        {
            if (_start == _end || _start < 0) return TagEnding.None;

            return _end < 0 ? TagEnding.SelfClosing : TagEnding.Closing;
        }
    }

    public TagEnding Unended
    {
        get
        {
            if (_start == _end) return TagEnding.None;

            if (_start < 0)
                return _end < 0 ? TagEnding.AttributeStart : TagEnding.Name;

            return _end < 0 ? TagEnding.SelfClosing : TagEnding.Closing;
        }
    }

    public TagEnding Ended
    {
        get
        {
            if (_start == _end) return TagEnding.None;

            if (_start < 0)
                return _end < 0 ? TagEnding.SelfClosingHasAttributes : TagEnding.ClosingHasAttributes;

            return _end < 0 ? TagEnding.SelfClosing : TagEnding.Closing;
        }
    }

    #endregion Props

    #region Ctors

    private Tag(int start, int end)
    {
        _start = start;
        _end = end;
    }

    private Tag(int start, int end, int offset)
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

    public Tag(int start, int end, TagEnding ending)
    {
        if (start < 0) throw new ArgumentOutOfRangeException(nameof(start));
        if (end <= start) throw new ArgumentOutOfRangeException(nameof(end));

        if (ending == TagEnding.Closing)
        {
            _start = start;
            _end = end;
        }
        else if (ending == TagEnding.SelfClosing)
        {
            _start = start;
            _end = ~end;
        }
        else if (ending == TagEnding.Name || ending == TagEnding.ClosingHasAttributes)
        {
            _start = ~start;
            _end = end;
        }
        else if (ending == TagEnding.AttributeStart || ending == TagEnding.SelfClosingHasAttributes)
        {
            _start = ~start;
            _end = ~end;
        }
        else
        {
            throw new ArgumentOutOfRangeException(nameof(ending));
        }
    }

    #endregion Ctors

    public Tag AddOffset(int offset) => new(_start, _end, offset);

    public bool TryGetOpening(out TagOpening opening)
    {
        if (_start == _end || _start < 0)
        {
            opening = default;
            return false;
        }
        opening = TagOpening.New(_start, _end);
        return true;
    }

    #region Comparison

    public int CompareTo(Tag other)
    {
        var compared = Start.CompareTo(other.Start);

        Debug.Assert(End.CompareTo(other.End) == compared);

        return compared;
    }

    public bool Equals(Tag other) => _start == other._start && _end == other._end;

    public override bool Equals(object? obj) => obj is Tag tag && Equals(tag);

    public override int GetHashCode() => HashCode.Combine(_start, _end);

    #endregion Comparison

    #region ToString

    public override string ToString()
    {
        Span<char> span = stackalloc char[4 + (2 * 10)];

        var status = TryFormat(span, out var written);

        Debug.Assert(status);

        return new(span.Slice(0, written));
    }

    public bool TryFormat(Span<char> chars, out int written)
    {
        const int minLength = 4;
        //<0..3>
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

    public static bool operator ==(Tag left, Tag right) => left.Equals(right);

    public static bool operator !=(Tag left, Tag right) => !left.Equals(right);

    public static bool operator <(Tag left, Tag right) => left.CompareTo(right) < 0;

    public static bool operator <=(Tag left, Tag right) => left.CompareTo(right) <= 0;

    public static bool operator >(Tag left, Tag right) => left.CompareTo(right) > 0;

    public static bool operator >=(Tag left, Tag right) => left.CompareTo(right) >= 0;


    public static explicit operator TagOpening(Tag tag) => TagOpening.New(tag._start, tag._end);

    #endregion Operators

    internal static Tag New(int start, int end) => new(start, end);
}