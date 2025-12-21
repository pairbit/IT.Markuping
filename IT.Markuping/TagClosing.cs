using System;
using System.Diagnostics;

namespace IT.Markuping;

//</10..13>
[DebuggerDisplay("{ToString(),nq}")]
public readonly struct TagClosing : IComparable<TagClosing>, IEquatable<TagClosing>, IFormattable
#if NET6_0_OR_GREATER
, ISpanFormattable
#endif
{
    private readonly int _start;
    private readonly int _end;

    public int Start => _start < 0 ? ~_start : _start;

    public int End => _end < 0 ? ~_end : _end;

    public int Length => End - Start;

    public bool IsEmpty => _start == _end;

    public Range Range => new(Start, End);

    //public bool HasNamespace => _start < 0;

    public bool HasSpace => _end < 0;

    private TagClosing(int start, int end, int offset)
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

    public TagClosing(int start, int end, bool hasSpace = false)
    {
        if (start < 0) throw new ArgumentOutOfRangeException(nameof(start));
        if (end <= start) throw new ArgumentOutOfRangeException(nameof(end));

        //_start = hasNamespace ? ~start : start;
        _start = start;
        _end = hasSpace ? ~end : end;
    }

    public TagClosing AddOffset(int offset) => new(_start, _end, offset);

    public int CompareTo(TagClosing other)
    {
        var compared = _start.CompareTo(other._start);

        Debug.Assert(_end.CompareTo(other._end) == compared);

        return compared;
    }

    public bool Equals(TagClosing other) => _start == other._start && _end == other._end;

    public override bool Equals(object? obj) => obj is TagClosing tag && Equals(tag);

    public override int GetHashCode() => HashCode.Combine(_start, _end);

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
        //</10..13>
        //minLength = 7-25
        if (chars.Length >= 7 && ((uint)Start).TryFormat(chars.Slice(2), out var startWritten))
        {
            if (chars.Length >= 5 + startWritten + startWritten &&
                ((uint)End).TryFormat(chars.Slice(4 + startWritten), out var endWritten))
            {
                written = startWritten + endWritten + 5;
                if (written <= chars.Length)
                {
                    chars[0] = '<';
                    chars[1] = '/';
                    chars[startWritten + 2] = '.';
                    chars[startWritten + 3] = '.';
                    chars[written - 1] = '>';
                    return true;
                }
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

    public static bool operator ==(TagClosing left, TagClosing right) => left.Equals(right);

    public static bool operator !=(TagClosing left, TagClosing right) => !left.Equals(right);

    #endregion Operators
}