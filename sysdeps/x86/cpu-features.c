/* Initialize CPU feature data.
   This file is part of the GNU C Library.
   Copyright (C) 2008-2016 Free Software Foundation, Inc.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

#include <cpuid.h>
#include <cpu-features.h>

static void
get_common_indeces (struct cpu_features *cpu_features,
		    unsigned int *family, unsigned int *model,
		    unsigned int *extended_model)
{
  if (family)
    {
      unsigned int eax;
      __cpuid (1, eax, cpu_features->cpuid[COMMON_CPUID_INDEX_1].ebx,
	       cpu_features->cpuid[COMMON_CPUID_INDEX_1].ecx,
	       cpu_features->cpuid[COMMON_CPUID_INDEX_1].edx);
      cpu_features->cpuid[COMMON_CPUID_INDEX_1].eax = eax;
      *family = (eax >> 8) & 0x0f;
      *model = (eax >> 4) & 0x0f;
      *extended_model = (eax >> 12) & 0xf0;
      if (*family == 0x0f)
	{
	  *family += (eax >> 20) & 0xff;
	  *model += *extended_model;
	}
    }

  if (cpu_features->max_cpuid >= 7)
    __cpuid_count (7, 0,
		   cpu_features->cpuid[COMMON_CPUID_INDEX_7].eax,
		   cpu_features->cpuid[COMMON_CPUID_INDEX_7].ebx,
		   cpu_features->cpuid[COMMON_CPUID_INDEX_7].ecx,
		   cpu_features->cpuid[COMMON_CPUID_INDEX_7].edx);

  /* Can we call xgetbv?  */
  if (CPU_FEATURES_CPU_P (cpu_features, OSXSAVE))
    {
      unsigned int xcrlow;
      unsigned int xcrhigh;
      asm ("xgetbv" : "=a" (xcrlow), "=d" (xcrhigh) : "c" (0));
      /* Is YMM and XMM state usable?  */
      if ((xcrlow & (bit_YMM_state | bit_XMM_state)) ==
	  (bit_YMM_state | bit_XMM_state))
	{
	  /* Determine if AVX is usable.  */
	  if (CPU_FEATURES_CPU_P (cpu_features, AVX))
	    cpu_features->feature[index_arch_AVX_Usable]
	      |= bit_arch_AVX_Usable;
	  /* Determine if AVX2 is usable.  */
	  if (CPU_FEATURES_CPU_P (cpu_features, AVX2))
	    cpu_features->feature[index_arch_AVX2_Usable]
	      |= bit_arch_AVX2_Usable;
	  /* Check if OPMASK state, upper 256-bit of ZMM0-ZMM15 and
	     ZMM16-ZMM31 state are enabled.  */
	  if ((xcrlow & (bit_Opmask_state | bit_ZMM0_15_state
			 | bit_ZMM16_31_state)) ==
	      (bit_Opmask_state | bit_ZMM0_15_state | bit_ZMM16_31_state))
	    {
	      /* Determine if AVX512F is usable.  */
	      if (CPU_FEATURES_CPU_P (cpu_features, AVX512F))
		{
		  cpu_features->feature[index_arch_AVX512F_Usable]
		    |= bit_arch_AVX512F_Usable;
		  /* Determine if AVX512DQ is usable.  */
		  if (CPU_FEATURES_CPU_P (cpu_features, AVX512DQ))
		    cpu_features->feature[index_arch_AVX512DQ_Usable]
		      |= bit_arch_AVX512DQ_Usable;
		}
	    }
	  /* Determine if FMA is usable.  */
	  if (CPU_FEATURES_CPU_P (cpu_features, FMA))
	    cpu_features->feature[index_arch_FMA_Usable]
	      |= bit_arch_FMA_Usable;
	}
    }
}

#ifdef __x86_64__
typedef long long op_t;
#else
typedef int op_t;
#endif

/* Return true if the first LEN bytes of strings A and B are the same
   where LEN != 0.  We can't use string/memory functions because they
   trigger an ifunc resolve loop.  */

static bool
equal (const char *a, const char *b, size_t len)
{
  size_t op_len = len % sizeof (op_t);
  if (op_len)
    {
      switch (op_len)
	{
	case 1:
	  if (*(char *) a != *(char *) b)
	    return false;
	  break;
	case 2:
	  if (*(short *) a != *(short *) b)
	    return false;
	  break;
	case 3:
	  if (*(short *) a != *(short *) b
	      || *(char *) (a + 2) != *(char *) (b + 2))
	    return false;
	  break;
#ifdef __x86_64__
	case 4:
	  if (*(int *) a != *(int *) b)
	    return false;
	  break;
	default:
	  if (*(int *) a != *(int *) b
	      || *(int *) (a + op_len - 4) != *(int *) (b + op_len - 4))
	    return false;
	  break;
#else
	default:
	  break;
#endif
	}
      /* Align length to size of op_t.  */
      len -= op_len;
      if (len == 0)
	return true;
      a += op_len;
      b += op_len;
    }

  /* Compare one op_t at a time.  */
  do
    {
      if (*(op_t *) a != *(op_t *) b)
	return false;
      len -= sizeof (op_t);
      if (len == 0)
	return true;
      a += sizeof (op_t);
      b += sizeof (op_t);
    }
  while (1);
}

/* Disable a CPU feature NAME.  We don't enable a CPU feature which isn't
   availble.  */
#define CHECK_GLIBC_IFUNC_CPU_OFF(name)					\
  if (equal (n, #name, sizeof (#name) - 1))				\
    {									\
      cpu_features->cpuid[index_cpu_##name].reg_##name			\
	&= ~bit_cpu_##name;						\
      break;								\
    }

/* Disable an ARCH feature NAME.  We don't enable an ARCH feature which
   isn't availble or has security implication.  */
#define CHECK_GLIBC_IFUNC_ARCH_OFF(name)				\
  if (equal (n, #name, sizeof (#name) - 1))				\
    {									\
      cpu_features->feature[index_arch_##name]				\
	&= ~bit_arch_##name;						\
      break;								\
    }

/* Enable/disable an ARCH feature NAME.  */
#define CHECK_GLIBC_IFUNC_ARCH_BOTH(name, disable)			\
  if (equal (n, #name, sizeof (#name) - 1))				\
    {									\
      if (disable)							\
	cpu_features->feature[index_arch_##name]			\
	  &= ~bit_arch_##name;						\
      else								\
	cpu_features->feature[index_arch_##name]			\
	  |= bit_arch_##name;						\
      break;								\
    }

/* Enable/disable an ARCH feature NAME.  Enable an ARCH feature only
   if the ARCH feature NEED is also enabled.  */
#define CHECK_GLIBC_IFUNC_ARCH_NEED_ARCH_BOTH(name, need, disable)	\
  if (equal (n, #name, sizeof (#name) - 1))				\
    {									\
      if (disable)							\
	cpu_features->feature[index_arch_##name]			\
	  &= ~bit_arch_##name;						\
      else if (CPU_FEATURES_ARCH_P (cpu_features, need))		\
	cpu_features->feature[index_arch_##name]			\
	  |= bit_arch_##name;						\
      break;								\
    }

/* Enable/disable an ARCH feature NAME.  Enable an ARCH feature only
   if the CPU feature NEED is also enabled.  */
#define CHECK_GLIBC_IFUNC_ARCH_NEED_CPU_BOTH(name, need, disable)	\
  if (equal (n, #name, sizeof (#name) - 1))				\
    {									\
      if (disable)							\
	cpu_features->feature[index_arch_##name]			\
	  &= ~bit_arch_##name;						\
      else if (CPU_FEATURES_CPU_P (cpu_features, need))			\
	cpu_features->feature[index_arch_##name]			\
	  |= bit_arch_##name;						\
      break;								\
    }

extern long int __x86_shared_non_temporal_threshold attribute_hidden;

static inline void
init_cpu_features (struct cpu_features *cpu_features, char **env)
{
  unsigned int ebx, ecx, edx;
  unsigned int family = 0;
  unsigned int model = 0;
  enum cpu_features_kind kind;

#if !HAS_CPUID
  if (__get_cpuid_max (0, 0) == 0)
    {
      kind = arch_kind_other;
      goto no_cpuid;
    }
#endif

  __cpuid (0, cpu_features->max_cpuid, ebx, ecx, edx);

  /* This spells out "GenuineIntel".  */
  if (ebx == 0x756e6547 && ecx == 0x6c65746e && edx == 0x49656e69)
    {
      unsigned int extended_model;

      kind = arch_kind_intel;

      get_common_indeces (cpu_features, &family, &model, &extended_model);

      if (family == 0x06)
	{
	  ecx = cpu_features->cpuid[COMMON_CPUID_INDEX_1].ecx;
	  model += extended_model;
	  switch (model)
	    {
	    case 0x1c:
	    case 0x26:
	      /* BSF is slow on Atom.  */
	      cpu_features->feature[index_arch_Slow_BSF]
		|= bit_arch_Slow_BSF;
	      break;

	    case 0x57:
	      /* Knights Landing.  Enable Silvermont optimizations.  */
	      cpu_features->feature[index_arch_Prefer_No_VZEROUPPER]
		|= bit_arch_Prefer_No_VZEROUPPER;

	    case 0x5c:
	    case 0x5f:
	      /* Unaligned load versions are faster than SSSE3
		 on Goldmont.  */

	    case 0x4c:
	      /* Airmont is a die shrink of Silvermont.  */

	    case 0x37:
	    case 0x4a:
	    case 0x4d:
	    case 0x5a:
	    case 0x5d:
	      /* Unaligned load versions are faster than SSSE3
		 on Silvermont.  */
#if index_arch_Fast_Unaligned_Load != index_arch_Prefer_PMINUB_for_stringop
# error index_arch_Fast_Unaligned_Load != index_arch_Prefer_PMINUB_for_stringop
#endif
#if index_arch_Fast_Unaligned_Load != index_arch_Slow_SSE4_2
# error index_arch_Fast_Unaligned_Load != index_arch_Slow_SSE4_2
#endif
#if index_arch_Fast_Unaligned_Load != index_arch_Fast_Unaligned_Copy
# error index_arch_Fast_Unaligned_Load != index_arch_Fast_Unaligned_Copy
#endif
	      cpu_features->feature[index_arch_Fast_Unaligned_Load]
		|= (bit_arch_Fast_Unaligned_Load
		    | bit_arch_Fast_Unaligned_Copy
		    | bit_arch_Prefer_PMINUB_for_stringop
		    | bit_arch_Slow_SSE4_2);
	      break;

	    default:
	      /* Unknown family 0x06 processors.  Assuming this is one
		 of Core i3/i5/i7 processors if AVX is available.  */
	      if ((ecx & bit_cpu_AVX) == 0)
		break;

	    case 0x1a:
	    case 0x1e:
	    case 0x1f:
	    case 0x25:
	    case 0x2c:
	    case 0x2e:
	    case 0x2f:
	      /* Rep string instructions, unaligned load, unaligned copy,
		 and pminub are fast on Intel Core i3, i5 and i7.  */
#if index_arch_Fast_Rep_String != index_arch_Fast_Unaligned_Load
# error index_arch_Fast_Rep_String != index_arch_Fast_Unaligned_Load
#endif
#if index_arch_Fast_Rep_String != index_arch_Prefer_PMINUB_for_stringop
# error index_arch_Fast_Rep_String != index_arch_Prefer_PMINUB_for_stringop
#endif
#if index_arch_Fast_Rep_String != index_arch_Fast_Unaligned_Copy
# error index_arch_Fast_Rep_String != index_arch_Fast_Unaligned_Copy
#endif
	      cpu_features->feature[index_arch_Fast_Rep_String]
		|= (bit_arch_Fast_Rep_String
		    | bit_arch_Fast_Unaligned_Load
		    | bit_arch_Fast_Unaligned_Copy
		    | bit_arch_Prefer_PMINUB_for_stringop);
	      break;
	    }
	}

      /* Unaligned load with 256-bit AVX registers are faster on
	 Intel processors with AVX2.  */
      if (CPU_FEATURES_ARCH_P (cpu_features, AVX2_Usable))
	cpu_features->feature[index_arch_AVX_Fast_Unaligned_Load]
	  |= bit_arch_AVX_Fast_Unaligned_Load;
    }
  /* This spells out "AuthenticAMD".  */
  else if (ebx == 0x68747541 && ecx == 0x444d4163 && edx == 0x69746e65)
    {
      unsigned int extended_model;

      kind = arch_kind_amd;

      get_common_indeces (cpu_features, &family, &model, &extended_model);

      ecx = cpu_features->cpuid[COMMON_CPUID_INDEX_1].ecx;

      unsigned int eax;
      __cpuid (0x80000000, eax, ebx, ecx, edx);
      if (eax >= 0x80000001)
	__cpuid (0x80000001,
		 cpu_features->cpuid[COMMON_CPUID_INDEX_80000001].eax,
		 cpu_features->cpuid[COMMON_CPUID_INDEX_80000001].ebx,
		 cpu_features->cpuid[COMMON_CPUID_INDEX_80000001].ecx,
		 cpu_features->cpuid[COMMON_CPUID_INDEX_80000001].edx);

      if (HAS_ARCH_FEATURE (AVX_Usable))
	{
	  /* Since the FMA4 bit is in COMMON_CPUID_INDEX_80000001 and
	     FMA4 requires AVX, determine if FMA4 is usable here.  */
	  if (CPU_FEATURES_CPU_P (cpu_features, FMA4))
	    cpu_features->feature[index_arch_FMA4_Usable]
	      |= bit_arch_FMA4_Usable;
	}

      if (family == 0x15)
	{
#if index_arch_Fast_Unaligned_Load != index_arch_Fast_Copy_Backward
# error index_arch_Fast_Unaligned_Load != index_arch_Fast_Copy_Backward
#endif
	  /* "Excavator"   */
	  if (model >= 0x60 && model <= 0x7f)
	    cpu_features->feature[index_arch_Fast_Unaligned_Load]
	      |= (bit_arch_Fast_Unaligned_Load
		  | bit_arch_Fast_Copy_Backward);
	}
    }
  else
    {
      kind = arch_kind_other;
      get_common_indeces (cpu_features, NULL, NULL, NULL);
    }

  /* Support i586 if CX8 is available.  */
  if (CPU_FEATURES_CPU_P (cpu_features, CX8))
    cpu_features->feature[index_arch_I586] |= bit_arch_I586;

  /* Support i686 if CMOV is available.  */
  if (CPU_FEATURES_CPU_P (cpu_features, CMOV))
    cpu_features->feature[index_arch_I686] |= bit_arch_I686;

#if !HAS_CPUID
no_cpuid:
#endif

  cpu_features->family = family;
  cpu_features->model = model;
  cpu_features->kind = kind;

  /* The current IFUNC selection is based on microbenchmarks in glibc.
     It should give the best performance for most workloads.  But other
     choices may have better performance for a particular workload or on
     the hardware which wasn't available when the selection was made.
     The environment variable, GLIBC_IFUNC=-xxx,yyy,-zzz...., can be
     used to enable CPU/ARCH feature yyy, disable CPU/ARCH feature yyy
     and zzz, where the feature name is case-sensitive and has to match
     the ones in cpu-features.h.  It can be used by glibc developers to
     tune for a new processor or override the IFUNC selection to improve
     performance for a particular workload.

     Since all CPU/ARCH features are hardware optimizations without
     security implication, except for Prefer_MAP_32BIT_EXEC, which can
     only be disabled, we check GLIBC_IFUNC for programs, including
     set*id ones.

     NOTE: the IFUNC selection may change over time.  Please check all
     multiarch implementations when experimenting.  */

  while (*env != NULL)
    {
      const char *p, *end;
      size_t len = sizeof ("GLIBC_IFUNC=");

      end = *env;
      for (p = end; *p != '\0'; p++)
	if (--len == 0 && equal (end, "GLIBC_IFUNC=",
				 sizeof ("GLIBC_IFUNC=") - 1))
	  {
	    /* Can't use strlen because it may trigger an ifunc resolve
	       loop.  */
	    for (end = p; *end != '\0'; end++);
	    do
	      {
		const char *c, *n;
		bool disable;
		size_t nl;

		for (c = p; *c != ','; c++)
		  if (c >= end)
		    break;

		len = c - p;
		disable = *p == '-';
		if (disable)
		  {
		    n = p + 1;
		    nl = len - 1;
		  }
		else
		  {
		    n = p;
		    nl = len;
		  }
		switch (nl)
		  {
		  default:
		    break;
		  case 3:
		    if (disable)
		      {
			CHECK_GLIBC_IFUNC_CPU_OFF (AVX);
			CHECK_GLIBC_IFUNC_CPU_OFF (CX8);
			CHECK_GLIBC_IFUNC_CPU_OFF (FMA);
			CHECK_GLIBC_IFUNC_CPU_OFF (HTT);
			CHECK_GLIBC_IFUNC_CPU_OFF (RTM);
		      }
		    break;
		  case 4:
		    if (disable)
		      {
			CHECK_GLIBC_IFUNC_CPU_OFF (AVX2);
			CHECK_GLIBC_IFUNC_CPU_OFF (CMOV);
			CHECK_GLIBC_IFUNC_CPU_OFF (ERMS);
			CHECK_GLIBC_IFUNC_CPU_OFF (FMA4);
			CHECK_GLIBC_IFUNC_CPU_OFF (SSE2);
			CHECK_GLIBC_IFUNC_ARCH_OFF (I586);
			CHECK_GLIBC_IFUNC_ARCH_OFF (I686);
		      }
		    break;
		  case 5:
		    if (disable)
		      {
			CHECK_GLIBC_IFUNC_CPU_OFF (SSSE3);
		      }
		    break;
		  case 6:
		    if (disable)
		      {
			CHECK_GLIBC_IFUNC_CPU_OFF (SSE4_1);
			CHECK_GLIBC_IFUNC_CPU_OFF (SSE4_2);
		      }
		    break;
		  case 7:
		    if (disable)
		      {
			CHECK_GLIBC_IFUNC_CPU_OFF (AVX512F);
			CHECK_GLIBC_IFUNC_CPU_OFF (OSXSAVE);
		      }
		    break;
		  case 8:
		    if (disable)
		      {
			CHECK_GLIBC_IFUNC_CPU_OFF (AVX512DQ);
			CHECK_GLIBC_IFUNC_CPU_OFF (POPCOUNT);
		      }
		    CHECK_GLIBC_IFUNC_ARCH_BOTH (Slow_BSF, disable);
		    break;
		  case 10:
		    if (disable)
		      {
			CHECK_GLIBC_IFUNC_ARCH_OFF (AVX_Usable);
			CHECK_GLIBC_IFUNC_ARCH_OFF (FMA_Usable);
		      }
		    break;
		  case 11:
		    if (disable)
		      {
			CHECK_GLIBC_IFUNC_ARCH_OFF (AVX2_Usable);
			CHECK_GLIBC_IFUNC_ARCH_OFF (FMA4_Usable);
		      }
		    CHECK_GLIBC_IFUNC_ARCH_BOTH (Prefer_ERMS, disable);
		    CHECK_GLIBC_IFUNC_ARCH_NEED_CPU_BOTH (Slow_SSE4_2,
							  SSE4_2,
							  disable);
		    break;
		  case 13:
		    if (disable)
		      {
			CHECK_GLIBC_IFUNC_ARCH_OFF (AVX512F_Usable);
		      }
		    CHECK_GLIBC_IFUNC_ARCH_NEED_ARCH_BOTH
		      (AVX_Fast_Unaligned_Load, AVX_Usable, disable);
		    break;
		  case 15:
		    if (disable)
		      {
			CHECK_GLIBC_IFUNC_ARCH_OFF (AVX512DQ_Usable);
		      }
		    CHECK_GLIBC_IFUNC_ARCH_BOTH (Fast_Rep_String, disable);
		    break;
		  case 18:
		    if (disable)
		      {
			if (equal (n, "non_temporal_store",
				   sizeof ("non_temporal_store") - 1))
			  {
			    /* Disable non-temporal store with
			       "-non_temporal_store".  */
			    __x86_shared_non_temporal_threshold
			      = (long int) -1;
			    break;
			  }
		      }
		    CHECK_GLIBC_IFUNC_ARCH_BOTH (Fast_Copy_Backward,
						 disable);
		    break;
		  case 19:
		    CHECK_GLIBC_IFUNC_ARCH_BOTH (Fast_Unaligned_Load,
						 disable);
		    CHECK_GLIBC_IFUNC_ARCH_BOTH (Fast_Unaligned_Copy,
						 disable);
		    break;
		  case 20:
		    CHECK_GLIBC_IFUNC_ARCH_NEED_ARCH_BOTH
		      (Prefer_No_VZEROUPPER, AVX_Usable, disable);
		    break;
		  case 21:
		    if (disable)
		      {
			CHECK_GLIBC_IFUNC_ARCH_OFF (Prefer_MAP_32BIT_EXEC);
		      }
		    break;
		  case 26:
		    CHECK_GLIBC_IFUNC_ARCH_NEED_CPU_BOTH
		      (Prefer_PMINUB_for_stringop, SSE2, disable);
		    break;
		  }
		p += len + 1;
	      }
	    while (p < end);
	    return;
	  }
      env++;
    }
}
