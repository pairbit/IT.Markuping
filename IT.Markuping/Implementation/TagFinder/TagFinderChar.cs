using System;

namespace IT.Markuping.Implementation;

public class TagFinderChar : TagFinder<char>
{
    public static readonly TagFinderChar Utf16 = new(new((MarkupTokens<char>)MarkupEncodingCharInfos.Utf16.Encoding));

#if NET
    public static readonly TagFinderChar Utf16_IgnoreCase = new(new((MarkupTokens<char>)MarkupEncodingCharInfos.Utf16.Encoding), StringComparison.OrdinalIgnoreCase);

    private readonly StringComparison _comparison;

    public TagFinderChar(Tokens tokens, StringComparison comparison = StringComparison.Ordinal) : base(tokens)
    {
        _comparison = comparison;
    }
#else
    public TagFinderChar(Tokens tokens) : base(tokens)
    {

    }
#endif
    protected override int IndexOf(ReadOnlySpan<char> data, ReadOnlySpan<char> value)
        => data.IndexOf(value
#if NET
            , _comparison
#endif
            );

    protected override int LastIndexOf(ReadOnlySpan<char> data, ReadOnlySpan<char> value)
        => data.LastIndexOf(value
#if NET
            , _comparison
#endif
            );
}