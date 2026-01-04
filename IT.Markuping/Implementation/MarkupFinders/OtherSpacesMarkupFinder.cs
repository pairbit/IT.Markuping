using System;

namespace IT.Markuping.Implementation;

public class OtherSpacesMarkupFinder<T> : MarkupFinder<T> where T : unmanaged, IEquatable<T>
{
    private readonly T _cr;//\r
    private readonly T _lf;//\n
    private readonly T _tab;//\t

    public OtherSpacesMarkupFinder(Tokens tokens, T cr, T lf, T tab) : base(tokens)
    {
        _cr = cr;
        _lf = lf;
        _tab = tab;
    }

    public OtherSpacesMarkupFinder(MarkupTokens<T> tokens) : base(new(tokens))
    {
        _cr = tokens._cr;
        _lf = tokens._lf;
        _tab = tokens._tab;
    }

    protected override bool IsSpace(T value) =>
        value.Equals(_tokens._space) || value.Equals(_lf) ||
        value.Equals(_cr) || value.Equals(_tab);
}