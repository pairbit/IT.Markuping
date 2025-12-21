using System;
using System.Text;

namespace IT.Markuping.Tests;

internal class TagData
{
    private byte[] NameBytes { get; }

    private byte[] NamespaceBytes { get; }

    private byte[] FullNameBytes { get; }

    public string NameString { get; }

    public string NamespaceString { get; }

    public string FullNameString { get; }

    public ReadOnlySpan<byte> Name => NameBytes;

    public ReadOnlySpan<byte> Namespace => NamespaceBytes;

    public ReadOnlySpan<byte> FullName => FullNameBytes;

    public bool HasNamespace => NamespaceString.Length > 0;

    public TagData(Encoding encoding, string name)
    {
        FullNameString = NameString = name;
        FullNameBytes = NameBytes = encoding.GetBytes(name);
        NamespaceString = string.Empty;
        NamespaceBytes = [];
    }

    public TagData(Encoding encoding, string name, string nameSpace)
    {
        NameString = name;
        NamespaceString = nameSpace;
        FullNameString = nameSpace + ":" + name;
        NameBytes = encoding.GetBytes(name);
        NamespaceBytes = encoding.GetBytes(nameSpace);
        FullNameBytes = encoding.GetBytes(FullNameString);
    }

    public override string ToString() => FullNameString;
}