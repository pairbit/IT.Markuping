using System;

namespace IT.Markuping.Implementation;

public class OtherSpacesTagFinder<T> : TagFinder<T> where T : unmanaged, IEquatable<T>
{
    private readonly T _cr;//\r
    private readonly T _lf;//\n
    private readonly T _tab;//\t

    public OtherSpacesTagFinder(Tokens tokens, T cr, T lf, T ht) : base(tokens)
    {
        _cr = cr;
        _lf = lf;
        _tab = ht;
    }

    public OtherSpacesTagFinder(MarkupTokens<T> tokens) : base(new(tokens))
    {
        _cr = tokens._cr;
        _lf = tokens._lf;
        _tab = tokens._tab;
    }

    protected override bool IsSpace(T value) =>
        value.Equals(_tokens._space) || value.Equals(_lf) ||
        value.Equals(_cr) || value.Equals(_tab);
}