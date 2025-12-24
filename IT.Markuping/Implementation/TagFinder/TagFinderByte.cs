using System;

namespace IT.Markuping.Implementation;

public class TagFinderByte : TagFinder<byte>
{
    private readonly bool[] _otherSpaces;

    public TagFinderByte(Tokens tokens, bool[] otherSpaces) : base(tokens)
    {
        if (otherSpaces == null) throw new ArgumentNullException(nameof(otherSpaces));
        if (otherSpaces.Length != 256) throw new ArgumentOutOfRangeException(nameof(otherSpaces));

        _otherSpaces = otherSpaces;
    }

    protected override bool IsSpace(byte value)
        => value.Equals(_tokens._space) || _otherSpaces[value];
}