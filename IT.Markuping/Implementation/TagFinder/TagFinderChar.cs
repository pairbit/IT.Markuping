#if NET
using System;
#endif

namespace IT.Markuping.Implementation;

public class TagFinderChar : OtherSpacesTagFinder<char>
{
    public static readonly TagFinderChar Utf16 = new((MarkupTokens<char>)MarkupAlphabets.Char.Utf16);
    public static readonly TagFinderChar Utf16BE = new((MarkupTokens<char>)MarkupAlphabets.Char.Utf16BE);
#if NET
    public static readonly TagFinderChar Utf16_IgnoreCase = new((MarkupTokens<char>)MarkupAlphabets.Char.Utf16, StringComparison.OrdinalIgnoreCase);

    private readonly StringComparison _comparison;
#endif

    public TagFinderChar(Tokens tokens, char cr, char lf, char tab
#if NET
        , StringComparison comparison = StringComparison.Ordinal
#endif
        ) : base(tokens, cr, lf, tab)
    {
#if NET
        _comparison = comparison;
#endif
    }

    public TagFinderChar(MarkupTokens<char> tokens
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

    protected override int IndexOf(ReadOnlySpan<char> data, ReadOnlySpan<char> value)
        => data.IndexOf(value, _comparison);

    protected override int LastIndexOf(ReadOnlySpan<char> data, ReadOnlySpan<char> value)
        => data.LastIndexOf(value, _comparison);
#endif
}