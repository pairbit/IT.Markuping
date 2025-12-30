using IT.Markuping.Implementation.TagStrategies;
using System;

namespace IT.Markuping.Implementation;

public class OtherSpaceTagStrategy<T> : TagStrategy<T> where T : unmanaged, IEquatable<T>
{
    private readonly T _otherSpace;

    public OtherSpaceTagStrategy(Tokens tokens, T otherSpace) : base(tokens)
    {
        _otherSpace = otherSpace;
    }

    public OtherSpaceTagStrategy(MarkupTokens<T> tokens) : base(new(tokens))
    {
        if (!tokens._cr.Equals(tokens._lf) || !tokens._cr.Equals(tokens._tab)) throw new ArgumentOutOfRangeException(nameof(tokens));

        _otherSpace = tokens._cr;
    }

    protected override bool IsSpace(T value)
        => value.Equals(_tokens._space) || value.Equals(_otherSpace);
}