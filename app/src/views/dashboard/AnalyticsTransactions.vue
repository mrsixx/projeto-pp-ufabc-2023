<script setup>
import { useCorrentistaStore } from '@/stores/CorrentistaStore';

const correntistaStore = useCorrentistaStore()

const statistics = [
  {
    title: 'Entradas',
    stats: correntistaStore.operacoesEntrada.length,
    icon: 'mdi-bank-transfer-in',
    color: 'primary',
  },
  {
    title: 'Maior entrada',
    stats: Math.max.apply(Math, correntistaStore.operacoesEntrada.map(o => o.valor)).toLocaleString('pt-BR', { style: 'currency', currency: 'BRL' }),
    icon: 'mdi-currency-usd',
    color: 'warning',
  },
  {
    title: 'Saídas',
    stats: correntistaStore.operacoesSaida.length,
    icon: 'mdi-bank-transfer-out',
    color: 'success',
  },
  {
    title: 'Maior saída',
    stats: Math.max.apply(Math, correntistaStore.operacoesSaida.map(o => o.valor)).toLocaleString('pt-BR', { style: 'currency', currency: 'BRL' }),
    icon: 'mdi-currency-usd-off',
    color: 'info',
  },
]
</script>

<template>
  <VCard>
    <VCardItem>
      <VCardTitle>Cheque Especial</VCardTitle>

      <template #append>
        <div class="me-n3">
          <MoreBtn />
        </div>
      </template>
    </VCardItem>

    <VCardText>
      <VProgressLinear
        :model-value="correntistaStore.chequeEspecialUtilizado"
        :max="150"
        height="20"
        reversed
        color="error"
      >
        <template v-slot:default="{ value }">
          <strong>{{ Math.ceil(value) }}%</strong>
        </template>
      </VProgressLinear>
      <h6 class="text-sm font-weight-medium mb-12">
        <span>Limite gasto este mês </span>
        <span class="font-weight-regular">{{correntistaStore.chequeEspecialUtilizadoFormatado}}</span>
      </h6>

      <VRow>
        <VCol
          v-for="item in statistics"
          :key="item.title"
          cols="6"
          sm="3"
        >
          <div class="d-flex align-center">
            <div class="me-3">
              <VAvatar
                :color="item.color"
                rounded
                size="42"
                class="elevation-1"
              >
                <VIcon
                  size="24"
                  :icon="item.icon"
                />
              </VAvatar>
            </div>

            <div class="d-flex flex-column">
              <span class="text-caption">
                {{ item.title }}
              </span>
              <span class="text-h6">{{ item.stats }}</span>
            </div>
          </div>
        </VCol>
      </VRow>
    </VCardText>
  </VCard>
</template>
