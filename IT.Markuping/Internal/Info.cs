#if NET || NETSTANDARD2_1_OR_GREATER
using System;
using System.Runtime.InteropServices;
using System.Text;

namespace IT.Markuping.Internal;

internal static class Info
{
    private static readonly Encoding Utf32BE = Encoding.GetEncoding(12001);

    public static string ToString<T>(ReadOnlySpan<T> span) where T : unmanaged
    {
        if (typeof(T) == typeof(byte)) return ToString(MemoryMarshal.AsBytes(span));
        if (typeof(T) == typeof(char)) return ToString(MemoryMarshal.Cast<T, char>(span));
        if (typeof(T) == typeof(short)) return ToString(MemoryMarshal.Cast<T, short>(span));
        if (typeof(T) == typeof(uint)) return ToString(MemoryMarshal.Cast<T, uint>(span));
        if (typeof(T) == typeof(int)) return ToString(MemoryMarshal.Cast<T, int>(span));

        throw new NotImplementedException($"Type '{typeof(T).FullName}' not mapped.");
    }

    public static string ToString(ReadOnlySpan<byte> span)
    {
        return Encoding.UTF8.GetString(span);
    }

    public static string ToString(ReadOnlySpan<char> span)
    {
        return span.ToString();
    }

    public static string ToString(ReadOnlySpan<short> span)
    {
        return Encoding.BigEndianUnicode.GetString(MemoryMarshal.AsBytes(span));
    }

    public static string ToString(ReadOnlySpan<uint> span)
    {
        return Encoding.UTF32.GetString(MemoryMarshal.AsBytes(span));
    }

    public static string ToString(ReadOnlySpan<int> span)
    {
        return Utf32BE.GetString(MemoryMarshal.AsBytes(span));
    }
}
#endif