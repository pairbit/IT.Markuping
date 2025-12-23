#if NET
using System;

namespace IT.Markuping.Implementation;

public class TagFinderChar : EquatableTagFinder<char>
{
    private readonly StringComparison _comparison;

    public TagFinderChar(Tokens tokens, StringComparison comparison) : base(tokens)
    {
        _comparison = comparison;
    }

    protected override int IndexOf(ReadOnlySpan<char> data, ReadOnlySpan<char> value)
        => data.IndexOf(value, _comparison);

    protected override int LastIndexOf(ReadOnlySpan<char> data, ReadOnlySpan<char> value)
        => data.LastIndexOf(value, _comparison);
}
#endif