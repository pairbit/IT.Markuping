using System;
using System.Diagnostics;

namespace IT.Markuping.Implementation.TagStrategies;

public class TagStrategyByte : TagStrategy<byte>
{
    private readonly bool[] _otherSpaces;

    public ReadOnlySpan<bool> OtherSpaces => _otherSpaces;

    public TagStrategyByte(Tokens tokens, bool[] otherSpaces) : base(tokens)
    {
        if (otherSpaces == null) throw new ArgumentNullException(nameof(otherSpaces));
        if (otherSpaces.Length != 256) throw new ArgumentOutOfRangeException(nameof(otherSpaces));

        _otherSpaces = otherSpaces;
    }

    protected override bool IsSpace(byte value)
    {
        Debug.Assert(_otherSpaces.Length == 256);

        return _otherSpaces[value];
    }
}