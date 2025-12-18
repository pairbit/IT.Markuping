using System;

namespace IT.Markuping;

public readonly struct Tag : IEquatable<Tag>
{
    private readonly int _start;
    private readonly int _end;

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

    internal Tag(int start, int end)
    {
        _start = start;
        _end = end;
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

    public override int GetHashCode() => HashCode.Combine(_start, _end);

    public override bool Equals(object? obj) => obj is Tag tag && Equals(tag);

    public bool Equals(Tag other) => _start == other._start && _end == other._end;

    public Tag AddOffset(int offset) => new(_start, _end, offset);

    public bool TryGetOpening(out TagOpening opening)
    {
        if (_start == _end || _start < 0)
        {
            opening = default;
            return false;
        }
        opening = new(_start, _end);
        return true;
    }

    public static bool operator ==(Tag left, Tag right) => left.Equals(right);

    public static bool operator !=(Tag left, Tag right) => !left.Equals(right);

    public static explicit operator TagOpening(Tag tag) => new(tag._start, tag._end);
}