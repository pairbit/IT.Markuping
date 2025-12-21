using System;
using System.Diagnostics;

namespace IT.Markuping;

//</10..13>
public readonly struct TagClosing : IEquatable<TagClosing>
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

    public override string ToString()
    {
        Span<char> span = stackalloc char[5 + (2 * 10)];

        var status = TryWrite(span, out var written);

        Debug.Assert(status);

        return new(span.Slice(0, written));
    }

    public override int GetHashCode() => HashCode.Combine(_start, _end);

    public override bool Equals(object? obj) => obj is TagClosing tag && Equals(tag);

    public bool TryWrite(Span<char> chars, out int written)
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

    public TagClosing AddOffset(int offset) => new(_start, _end, offset);

    public bool Equals(TagClosing other) => _start == other._start && _end == other._end;

    public static bool operator ==(TagClosing left, TagClosing right) => left.Equals(right);

    public static bool operator !=(TagClosing left, TagClosing right) => !left.Equals(right);
}