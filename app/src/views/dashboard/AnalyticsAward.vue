<script setup>
import { useCorrentistaStore } from '@/stores/CorrentistaStore';
import triangleDark from '@images/misc/triangle-dark.png'
import triangleLight from '@images/misc/triangle-light.png'
import trophy from '@images/misc/trophy.png'
import { useTheme } from 'vuetify'

const { global } = useTheme()
const triangleBg = computed(() => global.name.value === 'light' ? triangleLight : triangleDark)
const correntistaStore = useCorrentistaStore();
const usandoCheque = correntistaStore.saldo < 0
const showSaldo = ref(true)
</script>

<template>
  <VCard
    :title="`Olá, ${correntistaStore.nome}! 😎`"
    :subtitle="`Conta: ${correntistaStore.numConta}`"
    class="position-relative"
  >
    <VCardText>
      <h5 class="text-2xl font-weight-medium" :class="{'text-primary': !usandoCheque, 'text-error': usandoCheque, 'blur-saldo': !showSaldo}">
        {{correntistaStore.saldoFormatado}}
      </h5>
      <br/>
      <VBtn size="small" @click="showSaldo=!showSaldo" >
        {{ showSaldo ? 'Esconder' : 'Exibir' }} saldo
      </VBtn>
    </VCardText>

    <!-- Triangle Background -->
    <VImg
      :src="triangleBg"
      class="triangle-bg flip-in-rtl"
    />
  </VCard>
</template>

<style lang="scss">
@use "@layouts/styles/mixins" as layoutsMixins;

.v-card .triangle-bg {
  position: absolute;
  inline-size: 10.375rem;
  inset-block-end: 0;
  inset-inline-end: 0;
}

.v-card .trophy {
  position: absolute;
  inline-size: 4.9375rem;
  inset-block-end: 2rem;
  inset-inline-end: 2rem;
}

.blur-saldo {
  filter: blur(1.5rem);
}
</style>

