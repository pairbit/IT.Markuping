#if NET || NETSTANDARD2_1_OR_GREATER
using System;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Text;

namespace IT.Markuping.Internal;

internal static class Info
{
    private static readonly Encoding Utf32BE = Encoding.GetEncoding(12001);

    public static string ToString<T>(ReadOnlySpan<T> span) where T : unmanaged
    {
        if (typeof(T) == typeof(byte)) return ToString(MemoryMarshal.AsBytes(span));
        if (typeof(T) == typeof(char)) return span.ToString();
        if (typeof(T) == typeof(short)) return ToString(MemoryMarshal.Cast<T, short>(span));
        if (typeof(T) == typeof(uint)) return ToString(MemoryMarshal.Cast<T, uint>(span));
        if (typeof(T) == typeof(int)) return ToString(MemoryMarshal.Cast<T, int>(span));

        throw new NotImplementedException($"Type '{typeof(T).FullName}' not mapped.");
    }

    public static string ToString(ReadOnlySpan<byte> span)
    {
        var length = span.Length;
        if (length == 0)
            return string.Empty;

        if ((length % 2) != 0) 
            return Encoding.UTF8.GetString(span);//or EBCDIC

        if (length == 2)
        {
            //Utf16BE - 00 32
            if (span[0] == 0)
                return Encoding.BigEndianUnicode.GetString(span);

            //Utf16 - 32 00
            if (span[1] == 0)
                return Encoding.Unicode.GetString(span);

            return Encoding.UTF8.GetString(span);//or EBCDIC
        }

        Debug.Assert(length >= 4);

        //Utf16BE - 00 32 00 20
        //Utf32BE - 00 00 00 32
        if (span[0] == 0)
        {
            //Utf32BE - 00 00 00 32
            if (span[1] == 0)
            {
                Debug.Assert(span[2] == 0 && span[3] != 0);
                return Utf32BE.GetString(span);
            }
            //Utf16BE - 00 32 00 20
            Debug.Assert(span[2] == 0 && span[3] != 0);
            return Encoding.BigEndianUnicode.GetString(span);
        }
        //Utf16 - 32 00 20 00
        //Utf32 - 32 00 00 00
        else if (span[1] == 0)
        {
            //Utf32 - 32 00 00 00
            if (span[2] == 0)
            {
                Debug.Assert(span[3] == 0);
                return Encoding.UTF32.GetString(span);
            }
            //Utf16 - 32 00 20 00
            Debug.Assert(span[3] == 0);
            return Encoding.Unicode.GetString(span);
        }

        return Encoding.UTF8.GetString(span);//or EBCDIC
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