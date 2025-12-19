using System;

namespace IT.Markuping;

public readonly struct TagClosing : IEquatable<TagClosing>
{
    private readonly int _start;
    private readonly int _end;

    public int Start => _start < 0 ? ~_start : _start;

    public int End => _end < 0 ? ~_end : _end;

    public int Length => End - Start;

    public bool IsEmpty => _start == _end;

    public Range Range => new(Start, End);

    public bool HasNamespace => _start < 0;

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

    public TagClosing(int start, int end, bool hasNamespace, bool hasSpace = false)
    {
        if (start < 0) throw new ArgumentOutOfRangeException(nameof(start));
        if (end <= start) throw new ArgumentOutOfRangeException(nameof(end));

        _start = hasNamespace ? ~start : start;
        _end = hasSpace ? ~end : end;
    }

    public override int GetHashCode() => HashCode.Combine(_start, _end);

    public override bool Equals(object? obj) => obj is TagClosing tag && Equals(tag);

    public TagClosing AddOffset(int offset) => new(_start, _end, offset);

    public bool Equals(TagClosing other) => _start == other._start && _end == other._end;

    public static bool operator ==(TagClosing left, TagClosing right) => left.Equals(right);

    public static bool operator !=(TagClosing left, TagClosing right) => !left.Equals(right);
}