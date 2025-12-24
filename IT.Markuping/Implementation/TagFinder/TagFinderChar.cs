using System;

namespace IT.Markuping.Implementation;

public class TagFinderChar : EquatableTagFinder<char>
{
#if NET
    private readonly StringComparison _comparison;

    public TagFinderChar(Tokens tokens, StringComparison comparison) : base(tokens)
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