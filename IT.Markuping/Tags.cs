using System;
using System.Diagnostics;

namespace IT.Markuping;

[DebuggerDisplay("{ToString(),nq}")]
public readonly struct Tags : IComparable<Tags>, IEquatable<Tags>, IFormattable
#if NET6_0_OR_GREATER
, ISpanFormattable
#endif
{
    private readonly TagOpening _opening;
    private readonly TagClosing _closing;

    #region Props

    public TagOpening Opening => _opening;

    //TODO: нужно ли удалять признак IsTree при возврате???
    public TagClosing Closing => _closing;

    public Range Outer => new(_opening.Start, _closing.End);

    public int Length => _closing.End - _opening.Start;

    public bool IsEmpty => _closing.End == _opening.Start;

    public Range Inner => new(_opening.End, _closing.Start);

    public int InnerLength => _closing.Start - _opening.End;

    //public bool HasNamespace => _closing.HasNamespace;

    public bool HasAttributes => _opening.HasAttributes;

    public bool IsTree => _closing.IsTree;

    public bool HasSpaceAtEnd => _closing.HasSpace;

    #endregion Props

    public Tags(TagOpening opening, TagClosing closing)
    {
        if (opening.IsSelfClosing) throw new ArgumentException("SelfClosing", nameof(opening));

        var openingEnd = opening.End;
        if (opening.Start >= openingEnd) throw new ArgumentOutOfRangeException(nameof(opening), "Start >= End");

        var closingStart = closing.Start;
        if (closingStart >= closing.End) throw new ArgumentOutOfRangeException(nameof(closing), "Start >= End");
        if (openingEnd > closingStart) throw new ArgumentOutOfRangeException(nameof(closing), "openingEnd > closingStart");

        _opening = opening;
        _closing = closing;
    }

    public Tags(TagOpening opening, TagClosing closing, bool isTree)
    {
        if (opening.IsSelfClosing) throw new ArgumentException("SelfClosing", nameof(opening));

        //TODO: что делать если уже с признаком IsTree
        if (!isTree && closing.IsTree) throw new ArgumentException("Tree", nameof(closing));

        var openingEnd = opening.End;
        if (opening.Start >= openingEnd) throw new ArgumentOutOfRangeException(nameof(opening), "Start >= End");

        var closingStart = closing.Start;
        if (closingStart >= closing.End) throw new ArgumentOutOfRangeException(nameof(closing), "Start >= End");
        if (openingEnd > closingStart) throw new ArgumentOutOfRangeException(nameof(closing), "openingEnd > closingStart");

        _opening = opening;
        _closing = isTree ? closing.WithTree() : closing;
    }

    #region Comparison

    public int CompareTo(Tags other)
    {
        var compared = _opening.CompareTo(other._opening);

        Debug.Assert(_closing.CompareTo(other._closing) == compared);

        return compared;
    }

    public bool Equals(Tags other) => _opening.Equals(other._opening) && _closing.Equals(other._closing);

    public override bool Equals(object? obj) => obj is Tags tags && Equals(tags);

    public override int GetHashCode() => HashCode.Combine(_opening, _closing);

    #endregion Comparison

    #region ToString

    public override string ToString()
    {
        Span<char> span = stackalloc char[4 + (2 * 10) + 6 + (2 * 10)];

        var status = TryFormat(span, out var written);

        Debug.Assert(status);

        return new(span.Slice(0, written));
    }

    public bool TryFormat(Span<char> chars, out int written)
    {
        //<0..3></3..5>
        //<0..3></3..5 >
        //min-max = 13-50
        if (chars.Length >= 13 && _opening.TryFormat(chars, out var openingWritten))
        {
            if (chars.Length >= openingWritten + openingWritten &&
                _closing.TryFormat(chars.Slice(openingWritten), out var closingWritten))
            {
                written = openingWritten + closingWritten;
                return true;
            }
            else
            {
                //TODO: clear openingWritten
                //chars.Slice(0, openingWritten).Clear();
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

    public static bool operator ==(Tags left, Tags right) => left.Equals(right);

    public static bool operator !=(Tags left, Tags right) => !left.Equals(right);

    public static bool operator <(Tags left, Tags right) => left.CompareTo(right) < 0;

    public static bool operator <=(Tags left, Tags right) => left.CompareTo(right) <= 0;

    public static bool operator >(Tags left, Tags right) => left.CompareTo(right) > 0;

    public static bool operator >=(Tags left, Tags right) => left.CompareTo(right) >= 0;

    #endregion Operators
}