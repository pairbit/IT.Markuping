using System;

namespace IT.Markuping.Implementation;

public class OtherSpaceTagFinder<T> : TagFinder<T> where T : unmanaged, IEquatable<T>
{
    private readonly T _otherSpace;

    public OtherSpaceTagFinder(Tokens tokens, T otherSpace) : base(tokens)
    {
        _otherSpace = otherSpace;
    }

    public OtherSpaceTagFinder(MarkupTokens<T> tokens) : base(new(tokens))
    {
        if (!tokens._cr.Equals(tokens._lf) || !tokens._cr.Equals(tokens._tab)) throw new ArgumentOutOfRangeException(nameof(tokens));

        _otherSpace = tokens._cr;
    }

    protected override bool IsSpace(T value)
        => value.Equals(_tokens._space) || value.Equals(_otherSpace);
}