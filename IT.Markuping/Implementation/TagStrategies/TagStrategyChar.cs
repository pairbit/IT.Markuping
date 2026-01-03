#if NET
using System;
#endif

namespace IT.Markuping.Implementation.TagStrategies;

public class TagStrategyChar : OtherSpacesTagStrategy<char>
{
#if NET
    private readonly StringComparison _comparison;
#endif

    public TagStrategyChar(Tokens tokens, char cr, char lf, char tab
#if NET
        , StringComparison comparison = StringComparison.Ordinal
#endif
        ) : base(tokens, cr, lf, tab)
    {
#if NET
        _comparison = comparison;
#endif
    }

    public TagStrategyChar(MarkupTokens<char> tokens
#if NET
        , StringComparison comparison = StringComparison.Ordinal
#endif
        ) : base(tokens)
    {
#if NET
        _comparison = comparison;
#endif
    }

#if NET
    protected override bool Equals(ReadOnlySpan<char> data, ReadOnlySpan<char> value)
        => data.Equals(value, _comparison);

    public override int IndexOf(ReadOnlySpan<char> data, ReadOnlySpan<char> value)
        => data.IndexOf(value, _comparison);

    public override int LastIndexOf(ReadOnlySpan<char> data, ReadOnlySpan<char> value)
        => data.LastIndexOf(value, _comparison);
#endif
}