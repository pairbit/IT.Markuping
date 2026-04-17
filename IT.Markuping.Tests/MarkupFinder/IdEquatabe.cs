using IT.Markuping.Interfaces;
using System;
using System.Runtime.InteropServices;

namespace IT.Markuping.Tests;

internal class IdEquatabe<T> : INameEquatable where T : unmanaged, IEquatable<T>
{
    private readonly T _colon;
    private readonly T _I, _i, _d, _D;

    public IdEquatabe(MarkupAlphabet<T> alphabet)
    {
        var token = (MarkupTokens<T>)alphabet;
        _colon = token.Colon;
        _i = token.i;
        _I = token.I;
        _d = token.d;
        _D = token.D;
    }

    bool INameEquatable.Equals<T2>(ReadOnlySpan<T2> name)
    {
        if (typeof(T) == typeof(T2)) return Equals(MemoryMarshal.Cast<T2, T>(name));

        throw new NotImplementedException();
    }

    //id, Id, ID
    public bool Equals(ReadOnlySpan<T> name)
    {
        var index = name.IndexOf(_colon);
        if (index > -1)
        {
            name = name.Slice(index + 1);
        }

        if (name.Length == 2)
        {
            var first = name[0];
            if (first.Equals(_i))
                return name[1].Equals(_d);

            if (first.Equals(_I))
            {
                var second = name[1];
                return second.Equals(_d) || second.Equals(_D);
            }
        }

        return false;
    }
}