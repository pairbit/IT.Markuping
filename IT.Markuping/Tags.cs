using System;

namespace IT.Markuping;

public readonly struct Tags
{
    private readonly TagOpening _opening;
    private readonly TagClosing _closing;

    public TagOpening Opening => _opening;

    public TagClosing Closing => _closing;

    public Range Outer => new(_opening.Start, _closing.End);

    public int Length => _closing.End - _opening.Start;

    public bool IsEmpty => _closing.End == _opening.Start;

    public Range Inner => new(_opening.End, _closing.Start);

    public int InnerLength => _closing.Start - _opening.End;

    public bool HasNamespace => _closing.HasNamespace;

    public bool HasAttributes => _opening.HasAttributes;

    public Tags(TagOpening opening, TagClosing closing)
    {
        if (opening.IsSelfClosing) throw new ArgumentOutOfRangeException(nameof(opening));
        
        var openingEnd = opening.End;
        if (openingEnd <= opening.Start) throw new ArgumentOutOfRangeException(nameof(opening));

        var closingStart = closing.Start;
        if (closing.End <= closingStart) throw new ArgumentOutOfRangeException(nameof(closing));
        if (closingStart < openingEnd) throw new ArgumentOutOfRangeException(nameof(closing));

        _opening = opening;
        _closing = closing;
    }
}