#if NET
using System;
#endif

namespace IT.Markuping.Implementation;

public class MarkupFinderChar : OtherSpacesMarkupFinder<char>
{
#if NET
    private readonly StringComparison _comparison;
#endif

    public MarkupFinderChar(int[] codePages, Tokens tokens, char cr, char lf, char tab
#if NET
        , StringComparison comparison = StringComparison.Ordinal
#endif
        ) : base(codePages, tokens, cr, lf, tab)
    {
#if NET
        _comparison = comparison;
#endif
    }

    public MarkupFinderChar(int[] codePages, MarkupTokens<char> tokens
#if NET
        , StringComparison comparison = StringComparison.Ordinal
#endif
        ) : base(codePages, tokens)
    {
#if NET
        _comparison = comparison;
#endif
    }

#if NET
    protected override bool Equals(ReadOnlySpan<char> data, ReadOnlySpan<char> value)
        => data.Equals(value, _comparison);

    protected override int IndexOfTagName(ReadOnlySpan<char> data, ReadOnlySpan<char> value)
        => data.IndexOf(value, _comparison);

    protected override int LastIndexOfTagName(ReadOnlySpan<char> data, ReadOnlySpan<char> value)
        => data.LastIndexOf(value, _comparison);
#endif
}