<script setup>
import { useCorrentistaStore } from '@/stores/CorrentistaStore'
import americanBank from '@images/logos/american-bank.png'
import aws from '@images/logos/aws.png'
import citiBank from '@images/logos/citi-bank.png'
import digitalOcean from '@images/logos/digital-ocean.png'
import github from '@images/logos/github.png'
import google from '@images/logos/google.png'
import gumroad from '@images/logos/gumroad.png'
import mastercardLabel from '@images/logos/mastercard-label.png'
import slack from '@images/logos/slack.png'
import stripe from '@images/logos/stripe.png'
import AnalyticsBankStatementEntry from './AnalyticsBankStatementEntry.vue'
const correntistaStore = useCorrentistaStore()

</script>

<template>
  <VCard>
    <VRow no-gutters>
      <VCol
        cols="12"
        md="12"
      >
        <VCardItem>
          <VCardTitle>Extrato</VCardTitle>

          <template #append>
            <a
              href="javascript:void(0)"
              class="text-sm font-weight-medium"
            >View All</a>
          </template>
        </VCardItem>

        <VCardText>
          <VList class="card-list">
            <AnalyticsBankStatementEntry
              v-for="operacao in correntistaStore.operacoes"
              :key="operacao.id"
              :entry="operacao"
            />
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
