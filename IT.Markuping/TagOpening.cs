using System;

namespace IT.Markuping;

public readonly struct TagOpening : IEquatable<TagOpening>
{
    private readonly int _start;
    private readonly int _end;

    public int Start => _start < 0 ? ~_start : _start;

    public int End => _end < 0 ? ~_end : _end;

    public int Length => End - Start;

    public bool IsEmpty => _start == _end;

    public Range Range => new(Start, End);

    public bool HasAttributes => _start < 0;

    public bool IsSelfClosing => _end < 0;

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

    public TagOpening AddOffset(int offset) => new(_start, _end, offset);

    public override int GetHashCode() => HashCode.Combine(_start, _end);

    public override bool Equals(object? obj) => obj is TagOpening tag && Equals(tag);

    public bool Equals(TagOpening other) => _start == other._start && _end == other._end;

    public static bool operator ==(TagOpening left, TagOpening right) => left.Equals(right);

    public static bool operator !=(TagOpening left, TagOpening right) => !left.Equals(right);

    public static implicit operator Tag(TagOpening opening) => new(opening._start, opening._end);
}