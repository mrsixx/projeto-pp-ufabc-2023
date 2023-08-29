<script>
import AnalyticsAward from '@/views/dashboard/AnalyticsAward.vue'
import AnalyticsBarCharts from '@/views/dashboard/AnalyticsBarCharts.vue'
import AnalyticsDepositWithdraw from '@/views/dashboard/AnalyticsDepositWithdraw.vue'
import AnalyticsSalesByCountries from '@/views/dashboard/AnalyticsSalesByCountries.vue'
import AnalyticsTotalEarning from '@/views/dashboard/AnalyticsTotalEarning.vue'
import AnalyticsTotalProfitLineCharts from '@/views/dashboard/AnalyticsTotalProfitLineCharts.vue'
import AnalyticsTransactions from '@/views/dashboard/AnalyticsTransactions.vue'
import AnalyticsUserTable from '@/views/dashboard/AnalyticsUserTable.vue'
import AnalyticsWeeklyOverview from '@/views/dashboard/AnalyticsWeeklyOverview.vue'
import CardStatisticsVertical from '@core/components/cards/CardStatisticsVertical.vue'
import { useCorrentistaStore } from '@/stores/CorrentistaStore'
import { ref } from 'vue'

export default {
  components: {
    AnalyticsAward,
    AnalyticsBarCharts,
    AnalyticsDepositWithdraw,
    AnalyticsSalesByCountries,
    AnalyticsTotalEarning,
    AnalyticsTotalProfitLineCharts,
    AnalyticsTransactions,
    AnalyticsUserTable,
    AnalyticsWeeklyOverview,
    CardStatisticsVertical,
  },
  setup() {
    

    return {
      correntistaId: null,
      correntistaStore: useCorrentistaStore(),
    }
  },
  mounted() {
    this.correntistaId = parseInt(localStorage.getItem('pp-correntista-id'))
    if(!this.correntistaId)
      router.replace('/login')
    this.correntistaStore.load(this.correntistaId)
  },
  methods: {
    
  }
}
</script>


<template>
  <VRow class="match-height">
    <VCol
      cols="12"
      md="4"
    >
      <AnalyticsAward />
    </VCol>

    <VCol
      cols="12"
      md="8"
    >
      <AnalyticsTransactions />
    </VCol>

    <VCol
      cols="12"
      md="8"
    >
      <AnalyticsDepositWithdraw />
    </VCol>

    <VCol
      cols="12"
      md="4"
    >
      <AnalyticsTotalProfitLineCharts />
    </VCol>
  </VRow>
</template>
