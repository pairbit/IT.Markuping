using System;

namespace IT.Markuping.Interfaces;

//<!ATTLIST tag att ID #REQUIRED>
//<!ATTLIST tag att ID #IMPLIED>
public interface IAttName
{
    public bool Equals<T>(ReadOnlySpan<T> tagName, ReadOnlySpan<T> attName, ReadOnlySpan<T> dtd) where T : unmanaged;
}