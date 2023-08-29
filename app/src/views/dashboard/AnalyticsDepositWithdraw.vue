<script setup>
import { useCorrentistaStore } from '@/stores/CorrentistaStore'
import AnalyticsBankStatementEntry from './AnalyticsBankStatementEntry.vue'

const correntistaStore = useCorrentistaStore()
</script>

<template>
  <VCard>
    <VRow no-gutters>
      <VCol
        cols="12"
        md="6"
      >
        <VCardItem>
          <VCardTitle>Entradas</VCardTitle>
        </VCardItem>

        <VCardText>
          <VList class="card-list">
            <AnalyticsBankStatementEntry
              v-for="operacao in correntistaStore.operacoesEntrada.slice(0,5)"
              :key="operacao.id"
              :entry="operacao"/>
          </VList>
        </VCardText>
      </VCol>

      <VDivider
        :vertical="$vuetify.display.mdAndUp"
        :inset="$vuetify.display.mdAndUp"
      />

      <VCol
        cols="12"
        md="6"
      >
        <VCardItem>
          <VCardTitle>Saídas</VCardTitle>

          <template #append>
            <router-link
              to="/extrato"
              class="text-sm font-weight-medium"
            >Ver mais</router-link>
          </template>
        </VCardItem>

        <VCardText>
          <VList class="card-list">
            <AnalyticsBankStatementEntry
              v-for="operacao in correntistaStore.operacoesSaida.slice(0,5)"
              :key="operacao.id"
              :entry="operacao"/>
          </VList>
        </VCardText>
      </VCol>
    </VRow>
  </VCard>
</template>

<style lang="scss" scoped>
.card-list {
  --v-card-list-gap: 1.5rem;
}
</style>
